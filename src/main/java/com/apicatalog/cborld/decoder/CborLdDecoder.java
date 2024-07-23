package com.apicatalog.cborld.decoder;

import java.io.ByteArrayInputStream;
import java.math.BigInteger;
import java.net.URI;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.apicatalog.cborld.CborLdConstants;
import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.db.DbMappingProvider;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.cborld.mapper.Mapping;
import com.apicatalog.cborld.mapper.MappingProvider;
import com.apicatalog.cborld.mapper.TypeMap;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.DoublePrecisionFloat;
import co.nstant.in.cbor.model.HalfPrecisionFloat;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.NegativeInteger;
import co.nstant.in.cbor.model.SimpleValue;
import co.nstant.in.cbor.model.SinglePrecisionFloat;
import co.nstant.in.cbor.model.Special;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class CborLdDecoder implements DecoderConfig {

    protected Map<Integer, MappingProvider> providers;

    Collection<ValueDecoder> valueDecoders;
    
    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected boolean compactArrays;    
    protected URI base;

    public CborLdDecoder() {
        this(DefaultConfig.INSTANCE);
    }

    public CborLdDecoder(DecoderConfig config) {
        this.compactArrays = config.isCompactArrays();
        this.valueDecoders = config.valueDecoders();

        this.providers = new LinkedHashMap<>();
        this.providers.put(0x01, new DbMappingProvider());
        this.bundledContexts = DefaultConfig.STATIC_CONTEXTS;
        this.base = null;
        this.loader = null;
    }

    /**
     * If set to true, the encoder replaces arrays with just one element with that
     * element during encoding saving one byte. Enabled by default.
     *
     * @param enable <code>true</code> to enable arrays compaction
     * @return {@link CborLdDecoder} instance
     *
     */
    public CborLdDecoder compactArray(boolean enable) {
        compactArrays = enable;
        return this;
    }

    /**
     * Override any existing configuration by the given configuration set.
     * 
     * @param config a configuration set
     * @return {@link Encoder} instance
     */
    public CborLdDecoder config(DecoderConfig config) {
        this.compactArrays = config.isCompactArrays();
        this.valueDecoders = config.valueDecoders();
        return this;
    }

    /**
     * Set {@link DocumentLoader} used to fetch referenced JSON-LD contexts. If not
     * set then default document loader provided by {@link JsonLdOptions} is used.
     * 
     * @param loader a document loader to set
     * @return {@link CborLdDecoder} instance
     */
    public CborLdDecoder loader(DocumentLoader loader) {
        this.loader = loader;
        return this;
    }

    /**
     * Use well-known contexts that are bundled with the library instead of fetching
     * it online. <code>true</code> by default. Disabling might cause slower
     * processing.
     *
     * @param enable <code>true</code> to use static bundled contexts
     * @return {@link CborLdDecoder} instance
     */
    public CborLdDecoder useBundledContexts(boolean enable) {
        this.bundledContexts = enable;
        return this;
    }

    /**
     * If set, then is used as the input document's base IRI.
     *
     * @param base a document base
     * @return {@link CborLdDecoder} instance
     */
    public CborLdDecoder base(URI base) {
        this.base = base;
        return this;
    }
    
    /**
     * Associate new terms dictionary
     * 
     * @param code CBOR-LD terms dictionary code 
     * @param dictionary a dictionary instance
     * @return {@link CborLdDecoder} instance
     */
    public CborLdDecoder dictionary(int code, Dictionary dictionary) {
        //TODO
        return this;
    }

    /**
     * Associate new terms dictionary
     * 
     * @param code CBOR-LD terms dictionary code 
     * @param mapping terms dictionary provider
     * @return {@link CborLdDecoder} instance
     */
    public CborLdDecoder dictionary(int code, MappingProvider mapping) {
        //TODO
        return this;
    }
    
    /**
     * Decode CBOR-LD document as JSON-LD document.
     * 
     * @param encodedDocument an encoded CBOR-LD document
     * 
     * @return a decoded CBOR-LD document
     *
     * @throws ContextError
     * @throws DecoderError
     */
    public JsonValue decode(byte[] encodedDocument) throws ContextError, DecoderError {

        if (encodedDocument == null) {
            throw new IllegalArgumentException("The encoded document paramenter must not be null but byte array.");
        }

        if (encodedDocument.length < 4) {
            throw new DecoderError(Code.InvalidDocument,
                    "The encoded document must be at least 4 bytes but is [" + encodedDocument.length + "].");
        }

        if (encodedDocument[0] != CborLdConstants.CBOR_LD_LEADING_BYTE) {
            throw new DecoderError(Code.InvalidDocument, "The document is not CBOR-LD document. Must start with "
                    + Hex.toString(CborLdConstants.CBOR_LD_LEADING_BYTE)
                    + ", but is "
                    + Hex.toString(encodedDocument[0])
                    + ".");
        }

        if (encodedDocument[1] != CborLdConstants.CBOR_LD_VERSION_6_BYTE
                && encodedDocument[1] != CborLdConstants.CBOR_LD_VERSION_5_BYTE
                ) {
            throw new DecoderError(Code.InvalidDocument, "The document is not CBOR-LD document. Must start with "
                    + Hex.toString(CborLdConstants.CBOR_LD_VERSION_6_BYTE)
                    + ", or "
                    + Hex.toString(CborLdConstants.CBOR_LD_VERSION_5_BYTE)
                    + ", but is "
                    + Hex.toString(encodedDocument[1])
                    + ".");            
        }

        final MappingProvider mapping = providers.get(Byte.toUnsignedInt(encodedDocument[2]));
        
        if (mapping == null) {
            throw new DecoderError(Code.UnknownCompression,
                    "Unkknown CBOR-LD document terms dictionary type id, found ["
                            + Hex.toString(encodedDocument[2]) + "].");            
        }

        if (loader == null) {
            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
            ((HttpLoader) loader).fallbackContentType(MediaType.JSON);
        }
        
        return decode(encodedDocument, mapping, bundledContexts
                ? new StaticContextLoader(loader)
                : loader);
    }
    

    protected JsonValue decode(byte[] encoded, final MappingProvider provider, final DocumentLoader loader) throws ContextError, DecoderError {
        try {
            final ByteArrayInputStream bais = new ByteArrayInputStream(encoded);
            final List<DataItem> dataItems = new CborDecoder(bais).decode();

            // nothing do de-compress
            if (dataItems.isEmpty()) {
                return null;
            }

            // only one object
            if (dataItems.size() == 1) {
                return decodeCompressed(dataItems.iterator().next(), provider, loader);
            }

            // decode as an array of objects
            final JsonArrayBuilder builder = Json.createArrayBuilder();

            for (final DataItem item : dataItems) {

                builder.add(decodeCompressed(item, provider, loader));
            }

            return builder.build();

        } catch (final CborException e) {
            throw new DecoderError(Code.InvalidDocument, e);
        }
    }

    final JsonValue decodeCompressed(final DataItem data, final MappingProvider provider, final DocumentLoader loader) throws DecoderError, ContextError {
        final Mapping mapping = provider.getDecoderMapping(data, base, loader, this);
        return decodeData(data, null, mapping.typeMap(), mapping.dictionary());
    }

    protected final JsonValue decodeData(final DataItem data, final String term, final TypeMap def, final Dictionary index) throws DecoderError, ContextError {

        if (data == null) {
            throw new IllegalArgumentException("The data parameter must not be null.");
        }

        switch (data.getMajorType()) {
        case MAP:
            return decodeMap((co.nstant.in.cbor.model.Map) data, term != null ? def.getMapping(term) : def, index);

        case ARRAY:
            return decodeArray(((Array) data).getDataItems(), term, def, index);

        case UNICODE_STRING:
            return decodeString((UnicodeString) data, term);

        case UNSIGNED_INTEGER:
            return decodeInteger(data, term, def, index);

        case SPECIAL:
            return decode((Special) data);

        case NEGATIVE_INTEGER:
            return Json.createValue(((NegativeInteger) data).getValue());

        case BYTE_STRING:
            JsonValue decoded = decodeValue(data, term, def, index);
            if (decoded == null) {
                throw new DecoderError(Code.InvalidDocument, "Unknown encoded value [" + data.getMajorType() + "] at key [" + term + "].");
            }

            return decoded;

        default:
            throw new IllegalStateException("An unexpected data item type [" + data.getMajorType() + "].");
        }
    }

    protected final JsonObject decodeMap(final co.nstant.in.cbor.model.Map map, final TypeMap def, final Dictionary index) throws DecoderError, ContextError {

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
                    && !((UnsignedInteger) key).getValue().mod(BigInteger.ONE.add(BigInteger.ONE)).equals(BigInteger.ZERO);

            JsonValue json = null;
            String term = decodeKey(key, index);

            if (!isArray && MajorType.ARRAY.equals(value.getMajorType())) {
                json = decodeValue(value, term, def, index);
            }

            if (json == null) {
                json = decodeData(value, term, def, index);

                if (isArray
                        && compactArrays
                        && (JsonUtils.isNotArray(json)
                                || json.asJsonArray().size() == 1)) {

                    json = Json.createArrayBuilder().add(json).build();
                }
            }

            builder.add(decodeKey(key, index), json);
        }

        return builder.build();
    }

    protected static final String decodeKey(final DataItem data, final Dictionary index) {

        if (data == null) {
            throw new IllegalArgumentException("The data parameter must not be null.");
        }

        switch (data.getMajorType()) {
        case UNICODE_STRING:
            return decodeKey(((UnicodeString) data).getString());

        case UNSIGNED_INTEGER:
            return decodeKey(((UnsignedInteger) data).getValue(), index);

        default:
            return data.toString();
        }
    }

    protected static final String decodeKey(final String key) {
        return key;
    }

    protected static final String decodeKey(final BigInteger key, final Dictionary index) {

        if (key.mod(BigInteger.ONE.add(BigInteger.ONE)).equals(BigInteger.ZERO)) {
            String result = index.getValue(key);
            return result != null ? result : key.toString();
        }

        String result = index.getValue(key.subtract(BigInteger.ONE));

        return result != null ? result : key.toString();
    }

    protected final JsonArray decodeArray(final Collection<DataItem> items, final String key, final TypeMap def, final Dictionary index) throws DecoderError, ContextError {

        if (items == null) {
            throw new IllegalArgumentException("The items parameter must not be null.");
        }

        if (items.isEmpty()) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }

        final JsonArrayBuilder builder = Json.createArrayBuilder();

        for (final DataItem item : items) {
            builder.add(decodeData(item, key, def, index));
        }

        return builder.build();
    }

    protected static final JsonString decodeString(final UnicodeString string, final String key) {

        if (string == null) {
            throw new IllegalArgumentException("The string parameter must not be null.");
        }
        return Json.createValue(string.getString());
    }

    protected final JsonValue decodeInteger(final DataItem number, final String key, final TypeMap def, final Dictionary index) throws DecoderError {

        if (number == null) {
            throw new IllegalArgumentException("The number parameter must not be null.");
        }

        JsonValue decoded = decodeValue(number, key, def, index);

        if (decoded != null) {
            return decoded;
        }

        // fallback
        return Json.createValue(((UnsignedInteger) number).getValue());
    }

    protected final JsonValue decodeValue(final DataItem value, final String term, final TypeMap def, final Dictionary index) throws DecoderError {
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

    protected static final JsonValue decode(final Special value) {
        switch (value.getSpecialType()) {
        case IEEE_754_DOUBLE_PRECISION_FLOAT:
            return Json.createValue(((DoublePrecisionFloat) value).getValue());

        case IEEE_754_HALF_PRECISION_FLOAT:
            return Json.createValue(((HalfPrecisionFloat) value).getValue());

        case IEEE_754_SINGLE_PRECISION_FLOAT:
            return Json.createValue(((SinglePrecisionFloat) value).getValue());

        case SIMPLE_VALUE:
            return decode((SimpleValue) value);

        default:
            break;
        }

        throw new IllegalStateException("Unsupported CBOR special type [" + value.getSpecialType() + "].");
    }

    protected static final JsonValue decode(final SimpleValue value) {
        switch (value.getSimpleValueType()) {
        case FALSE:
            return JsonValue.FALSE;

        case TRUE:
            return JsonValue.TRUE;

        case NULL:
            return JsonValue.NULL;

        default:
            break;
        }

        throw new IllegalStateException("Unsupported CBOR simple value type [" + value.getSimpleValueType() + "].");
    }

    @Override
    public boolean isCompactArrays() {
        return compactArrays;
    }

    @Override
    public Collection<ValueDecoder> valueDecoders() {
        return valueDecoders;
    }
}
