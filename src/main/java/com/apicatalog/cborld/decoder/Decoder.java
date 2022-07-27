package com.apicatalog.cborld.decoder;

import java.io.ByteArrayInputStream;
import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.dictionary.ContextDictionary;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.hex.Hex;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
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

public class Decoder {

    protected final byte[] encoded;
    protected final boolean compressed;
    protected final DocumentLoader loader;

    protected CodeTermMap index;
    protected Dictionary contexts;

    protected Decoder(byte[] encoded, boolean compressed, final DocumentLoader loader) {
        this.encoded = encoded;
        this.compressed = compressed;
        this.loader = loader;
        this.contexts = new ContextDictionary();    //FIXME
    }

    public static final Decoder create(byte[] encodedDocument, DocumentLoader loader) throws DecoderError {
    
        if (encodedDocument == null) {
            throw new IllegalArgumentException("The encoded document paramenter must not be null but byte arrayy.");
        }
    
        if (encodedDocument.length < 4) {
            throw new DecoderError(Code.InvalidDocument,
                "The encoded document must be at least 4 bytes but is [" + encodedDocument.length + "].");
        }
    
        if (encodedDocument[0] != CborLd.CBOR_LD_BYTE_PREFIX[0]
            || encodedDocument[1] != CborLd.CBOR_LD_BYTE_PREFIX[1]) {
            throw new DecoderError(Code.InvalidDocument, "The document is not CBOR-LD document.");
        }
    
        if (encodedDocument[2] == CborLd.COMPRESSED) {
            return new Decoder(encodedDocument, true, loader);
        }
    
        if (encodedDocument[2] == CborLd.UNCOMPRESSED) {
            return new Decoder(encodedDocument, false, loader);
        }
    
        throw new DecoderError(Code.UnknownCompression,
            "Unkknown CBOR-LD document compression, expected 0x00 - uncompressed or 0x01 - compressed, but found ["
                + Hex.toString(encodedDocument[2]) + "].");
    }

    public JsonValue decode() throws DecoderError, ContextError {
        if (compressed) {
            return decodeCompressed();
        }
        return decodeUncompressed();
    }

    final JsonValue decodeCompressed() throws DecoderError, ContextError {
    
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
    
        Collection<String> contextUrls = (new DecoderContext(new ContextDictionary())).get(data);
    
        this.index = CodeTermMap.from(contextUrls, loader);
    
        return decodeData(data, null, null);
    }

    final JsonValue decodeData(final DataItem data, final String key, TermDefinition def) throws DecoderError {
    
        if (data == null) {
            throw new IllegalArgumentException("The data parameter must not be null.");
        }
    
        switch (data.getMajorType()) {
        case MAP:
            return decodeMap((Map) data, def);
    
        case ARRAY:
            return decodeArray(((Array) data).getDataItems(), key, def);
    
        case UNICODE_STRING:
            return decodeString((UnicodeString) data, key);
    
        case UNSIGNED_INTEGER:
            return decodeInteger(((UnsignedInteger)data).getValue(), key, def);
    
        default:
            throw new IllegalStateException("An unexpected data item type [" + data.getMajorType() + "].");
        }
    }

    final JsonObject decodeMap(final Map map, TermDefinition def) throws DecoderError {
    
        if (map == null) {
            throw new IllegalArgumentException("The map parameter must not be null.");
        }
    
        if (map.getKeys().isEmpty()) {
            return JsonValue.EMPTY_JSON_OBJECT;
        }
    
        final JsonObjectBuilder builder = Json.createObjectBuilder();
    
        for (final DataItem key : map.getKeys()) {
    
            builder.add(decodeKey(key), decodeData(map.get(key), decodeKey(key), index.getDefinition(def, decodeKey(key))));
        }
    
        return builder.build();
    }

    final JsonArray decodeArray(final Collection<DataItem> items, String key, TermDefinition def) throws DecoderError {
    
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

    final String decodeKey(final DataItem data) throws DecoderError {
    
        if (data == null) {
            throw new IllegalArgumentException("The data parameter must not be null.");
        }
    
        switch (data.getMajorType()) {
        case UNICODE_STRING:
            return decodeKey(((UnicodeString)data).getString());
    
        case UNSIGNED_INTEGER:
            return decodeKey(((UnsignedInteger)data).getValue());
    
        default:
                throw new DecoderError(Code.Unsupported, "A property name of type [" + data.getMajorType() +"] is not supported.");
        }
    }

    final String decodeKey(String key) {
        //TODO
        return key;
    }

    final String decodeKey(BigInteger key) {
    
        String result = index.getValue(key);
    
        //TODO
        return result != null ? result : key.toString();
    }

    final JsonString decodeString(final UnicodeString string, final String key) {
    
        if (string == null) {
            throw new IllegalArgumentException("The string parameter must not be null.");
        }
        //TODO
        return Json.createValue(string.getString());
    }

    final JsonValue decodeInteger(final BigInteger number, String key, TermDefinition def) {
    
        if (number == null) {
            throw new IllegalArgumentException("The number parameter must not be null.");
        }
    
    
        if (Keywords.CONTEXT.equals(key)) {
            final String context  = contexts.getValue(number);
            if (context != null) {
            return Json.createValue(context);
            } else {
            //TODO throw something
            }
        }
        if (def != null) {
            if (Keywords.TYPE.equals(def.getUriMapping())) {
                    String term = index.getValue(number);
                    if (term != null) {
                    return Json.createValue(term);
                    }
            }
        }
    
        //TODO
        return Json.createValue(number);
    }

    final JsonValue decodeUncompressed() throws DecoderError {
        /// TODO
        return null;
    }
}
