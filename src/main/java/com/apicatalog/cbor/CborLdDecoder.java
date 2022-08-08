package com.apicatalog.cbor;

import java.io.ByteArrayInputStream;
import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.mapper.Mapping;
import com.apicatalog.cborld.mapper.TypeMap;
import com.apicatalog.jsonld.json.JsonUtils;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class CborLdDecoder {

    @FunctionalInterface
    public interface CborValueDecoder {
        JsonValue decode(DataItem value, String term, Collection<String> path);
    }

    public JsonValue decode() throws ContextError, DecoderError {
            
        try {
            final ByteArrayInputStream bais = new ByteArrayInputStream(encoded);
            final List<DataItem> dataItems = new CborDecoder(bais).decode();
    
            // nothing do de-compress
            if (dataItems.isEmpty()) {
                return null;
            }
            
            // decode as an array of objects
            if (dataItems.size() > 1) {
    
                final JsonArrayBuilder builder = Json.createArrayBuilder();
        
                for (final DataItem item : dataItems) {

                    builder.add(decodeCompressed(item));
                }
        
                return builder.build();
            }
    
            return decodeCompressed(dataItems.iterator().next());
    
        } catch (final CborException e) {
            throw new DecoderError(Code.InvalidDocument, e);            
        }
    }

    final JsonValue decodeCompressed(final DataItem data) throws DecoderError, ContextError {
  
        final Mapping mapping = provider.getDecoderMapping(data, base, loader, this);
            
        index = mapping.dictionary();

        return decodeData(data, null, mapping.typeMap());
    }

    final JsonValue decodeData(final DataItem data, final String term, final TypeMap def) throws DecoderError, ContextError {
    
        if (data == null) {
            throw new IllegalArgumentException("The data parameter must not be null.");
        }
    
        switch (data.getMajorType()) {
        case MAP:
            return decodeMap((Map) data, term != null ? def.getMapping(term) : def);
    
        case ARRAY:
            return decodeArray(((Array) data).getDataItems(), term, def);
    
        case UNICODE_STRING:
            return decodeString((UnicodeString) data, term);
    
        case UNSIGNED_INTEGER:
            return decodeInteger(data, term, def);
            
        case BYTE_STRING:
            JsonValue decoded = decodeValue(data, term, def);
            if (decoded == null) {
                throw new DecoderError(Code.InvalidDocument, "Unknown encoded value [" + data.getMajorType() + "] at key [" + term + "].");
            }
            
            return decoded;
    
        default:
            throw new IllegalStateException("An unexpected data item type [" + data.getMajorType() + "].");
        }
    }

    final JsonObject decodeMap(final Map map, final TypeMap def) throws DecoderError, ContextError {
    
        if (map == null) {
            throw new IllegalArgumentException("The map parameter must not be null.");
        }
    
        if (map.getKeys().isEmpty()) {
            return JsonValue.EMPTY_JSON_OBJECT;
        }
    
        final JsonObjectBuilder builder = Json.createObjectBuilder();
    
        for (final DataItem key : map.getKeys()) {
    
            final DataItem value = map.get(key);
            
            boolean isArray = MajorType.UNSIGNED_INTEGER.equals(key.getMajorType())
                                && !((UnsignedInteger)key).getValue().mod(BigInteger.ONE.add(BigInteger.ONE)).equals(BigInteger.ZERO)
                                ;
            
            JsonValue json = null;
            String term = decodeKey(key);
            
            if (!isArray && MajorType.ARRAY.equals(value.getMajorType())) {
                json = decodeValue(value, term, def);
            }
            
            if (json == null) {
                json = decodeData(value, term, def);
                
                if (isArray 
                        && compactArrays
                        && (JsonUtils.isNotArray(json)
                                || json.asJsonArray().size() == 1
                                )
                        ) {
                    
                    json = Json.createArrayBuilder().add(json).build();
                }
            }
            
            
            builder.add(decodeKey(key), json);
        }
    
        return builder.build();
    }
    
    final String decodeKey(final DataItem data) {

        if (data == null) {
            throw new IllegalArgumentException("The data parameter must not be null.");
        }
    
        switch (data.getMajorType()) {
        case UNICODE_STRING:
            return ((UnicodeString)data).getString();
    
        case UNSIGNED_INTEGER:
            return decodeKey(((UnsignedInteger)data).getValue());

        default:
            return data.toString();
        }
    }

    final String decodeKey(final BigInteger key) {
        
        if (key.mod(BigInteger.ONE.add(BigInteger.ONE)).equals(BigInteger.ZERO)) {
            String result = index.getValue(key);
            return result != null ? result : key.toString();
        }        
    
        String result = index.getValue(key.subtract(BigInteger.ONE));

        return result != null ? result : key.toString();
    }

    final JsonArray decodeArray(final Collection<DataItem> items, final String key, final TypeMap def) throws DecoderError, ContextError {
    
        if (items == null) {
            throw new IllegalArgumentException("The items parameter must not be null.");
        }
    
        if (items.isEmpty()) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }
    
        final JsonArrayBuilder builder = Json.createArrayBuilder();
    
        for (final DataItem item : items) {
            builder.add(decodeData(item, key, def));
        }
    
        return builder.build();
    }

    final JsonString decodeString(final UnicodeString string, final String key) {
    
        if (string == null) {
            throw new IllegalArgumentException("The string parameter must not be null.");
        }
        return Json.createValue(string.getString());
    }

    final JsonValue decodeInteger(final DataItem number, final String key, final TypeMap def) throws DecoderError {
    
        if (number == null) {
            throw new IllegalArgumentException("The number parameter must not be null.");
        }
    
        JsonValue decoded = decodeValue(number, key, def);

        if (decoded != null) {
            return decoded;
        }
        
        // fallback
        return Json.createValue(((UnsignedInteger)number).getValue());
    }

    final JsonValue decodeValue(final DataItem value, final String term, final TypeMap def) throws DecoderError {
        if (def != null) { 
            final Collection<String> types = def.getType(term);

            for (final ValueDecoder decoder : valueDecoders) {
                final JsonValue decoded = decoder.decode(index, value, term, types);
                
                if (decoded != null) {
                    return decoded;
                }            
            }
        }
        return null;
    }
}
