package com.apicatalog.cborld.decoder;

import java.io.ByteArrayInputStream;
import java.math.BigInteger;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.config.DefaulConfig;
import com.apicatalog.cborld.context.Context;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.context.TypeMapping;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.cbor.CborCursor;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;

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

public class Decoder {

    protected final byte[] encoded;
    protected final boolean compressed;
        
    protected CodeTermMap index;

    protected DefaultTypeKeyNameMapper typeMap;
    
    // options
    protected Collection<ValueDecoder> valueDecoders;
    protected boolean compactArrays;
    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected URI base;
    
    protected Decoder(byte[] encoded, boolean compressed) {
        this.encoded = encoded;
        this.compressed = compressed;
        
        // default options
        this.valueDecoders = DefaulConfig.VALUE_DECODERS;
        this.compactArrays = DefaulConfig.COMPACT_ARRAYS;
        this.bundledContexts = DefaulConfig.STATIC_CONTEXTS;
        this.base = null;
        this.loader = null;
    }

    /**
     * If set to true, the encoder replaces arrays with
     * just one element with that element during encoding saving one byte.
     * Enabled by default.
     *
     * @param enable <code>true</code> to enable arrays compaction
     * @return {@link Decoder} instance
     *
     */
    public Decoder compactArray(boolean enable) {
        compactArrays = enable;
        return this;
    }

    /**
     * Set {@link DocumentLoader} used to fetch referenced JSON-LD contexts. 
     * If not set then default document loader provided by {@link JsonLdOptions} is used. 
     * 
     * @param loader a document loader to set
     * @return {@link Decoder} instance
     */
    public Decoder loader(DocumentLoader loader) {
        this.loader = loader;
        return this;
    }

    
    /**
     * Use well-known contexts that are bundled with the library instead of fetching it online.
     * <code>true</code> by default. Disabling might cause slower processing.
     *
     * @param enable
     * @return {@link Decoder} instance
     */
    public Decoder useBundledContexts(boolean enable) {
        this.bundledContexts = enable;
        return this;
    }
    
    /**
     * If set, then is used as the input document's base IRI.
     *
     * @param base
     * @return {@link Decoder} instance
     */
    public Decoder base(URI base) {
       this.base = base;
       return this;
    }
    
    public static final Decoder create(byte[] encodedDocument) throws DecoderError {
    
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
            return new Decoder(encodedDocument, true);
        }
    
        if (encodedDocument[2] == CborLd.UNCOMPRESSED) {
            return new Decoder(encodedDocument, false);
        }
    
        throw new DecoderError(Code.UnknownCompression,
            "Unkknown CBOR-LD document compression, expected 0x00 - uncompressed or 0x01 - compressed, but found ["
                + Hex.toString(encodedDocument[2]) + "].");
    }
    
    /**
     * Decode  CBOR-LD document as JSON-LD document.
     * 
     * @return a decoded CBOR-LD document
     * 
     * @throws DecoderError 
     * @throws ContextError
     */
    public JsonValue decode() throws DecoderError, ContextError {

        if (loader == null) {
            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
            ((HttpLoader)loader).setFallbackContentType(MediaType.JSON);
        }
        
        if (bundledContexts) {
            loader = new StaticContextLoader(loader);
        }
        
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
  
        try {

            typeMap = new DefaultTypeKeyNameMapper();
            
            index = CodeTermMap.create();

            final MapCursor cursor = CborCursor.from(
                    data, 
                    this::decodeKey, 
                    this::encodeKey,
                    this::decodeValue
                    );
    
            final Context context = Context.from(cursor, base, loader, index::add, typeMap);
            
            return decodeData(data, null, context.getTypeMapping());

        } catch (JsonLdError e) {
            throw new DecoderError(Code.InvalidDocument, e);
        }
    
    }

    final JsonValue decodeData(final DataItem data, final String term, TypeMapping def) throws DecoderError, ContextError {
    
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

    final JsonObject decodeMap(final Map map, TypeMapping def) throws DecoderError, ContextError {
    
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
                                && !((UnsignedInteger)key).getValue().mod(BigInteger.TWO).equals(BigInteger.ZERO)
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

    final JsonArray decodeArray(final Collection<DataItem> items, String key, TypeMapping def) throws DecoderError, ContextError {
    
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

    final String decodeKey(final DataItem data) {

        if (data == null) {
            throw new IllegalArgumentException("The data parameter must not be null.");
        }
    
        switch (data.getMajorType()) {
        case UNICODE_STRING:
            return decodeKey(((UnicodeString)data).getString());
    
        case UNSIGNED_INTEGER:
            return decodeKey(((UnsignedInteger)data).getValue());

        default:
            return data.toString();
//        default:
//            //TODO log throw new ContextError(com.apicatalog.cborld.context.ContextError.Code.Unsupported, "A property name of type [" + data.getMajorType() +"] is not supported.");
        }
    }

    final String decodeKey(String key) {
        //TODO
        return key;
    }

    final String decodeKey(BigInteger key) {
    
        if (key.mod(BigInteger.TWO).equals(BigInteger.ZERO)) {
            String result = index.getValue(key);
            return result != null ? result : key.toString();
        }        
    
        String result = index.getValue(key.subtract(BigInteger.ONE));

        //TODO
        return result != null ? result : key.toString();
    }
    
    final DataItem encodeKey(String key) {
        
        final BigInteger encodedProperty = index.getCode(key);
        
        if (encodedProperty != null) {
            return new UnsignedInteger(encodedProperty);
        }
        return new UnicodeString(key);
    }

    final JsonString decodeString(final UnicodeString string, final String key) {
    
        if (string == null) {
            throw new IllegalArgumentException("The string parameter must not be null.");
        }
        //TODO
        return Json.createValue(string.getString());
    }

    final JsonValue decodeInteger(final DataItem number, String key, TypeMapping def) throws DecoderError {
    
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

    final JsonValue decodeUncompressed() throws DecoderError {
        /// TODO
        return null;
    }
    
    final DataItem decodeValue(final DataItem value, String term, Collection<String> path) {

        //FIXME
        Collection<String> TYPE = Arrays.asList(Keywords.TYPE); 
        
        for (final ValueDecoder decoder : valueDecoders) {
            try {
                final JsonValue decoded = decoder.decode(index, value, term, 
                        typeMap.isTypeKey(term, path)
                        ? TYPE
                        : Collections.emptySet()
                        );
                
                if (decoded == null) {
                    continue;
                }
                
                if (JsonUtils.isString(decoded)) {
                    return new UnicodeString(((JsonString)decoded).getString());
                }
                
            } catch (DecoderError e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            
        }
        
        return value;
    }

    final JsonValue decodeValue(final DataItem value, String term, TypeMapping typeMapping) throws DecoderError {
        
        if (typeMapping != null) { 
            final Collection<String> types = typeMapping.getType(term);

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
