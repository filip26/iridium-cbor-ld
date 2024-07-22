package com.apicatalog.cborld.decoder;

import java.math.BigInteger;
import java.net.URI;
import java.util.Collection;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.config.DictionaryAlgorithm;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.cborld.mapper.MappingProvider;
import com.apicatalog.cborld.mapper.TypeMap;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.DoublePrecisionFloat;
import co.nstant.in.cbor.model.HalfPrecisionFloat;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.Map;
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

public abstract class Decoder implements DecoderConfig {

    protected final byte[] encoded;

    protected MappingProvider provider;
    protected Dictionary index;

    // options
    protected Collection<ValueDecoder> valueDecoders;
    protected boolean compactArrays;
    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected URI base;

    protected Decoder(byte[] encoded) {
        this.encoded = encoded;

        // default options
        config(DefaultConfig.INSTANCE);

        this.bundledContexts = DefaultConfig.STATIC_CONTEXTS;
        this.base = null;
        this.loader = null;
    }

    /**
     * If set to true, the encoder replaces arrays with just one element with that
     * element during encoding saving one byte. Enabled by default.
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
     * Override any existing configuration by the given configuration set.
     * 
     * @param config a configuration set
     * @return {@link Encoder} instance
     */
    public Decoder config(DecoderConfig config) {
        this.compactArrays = config.isCompactArrays();
        this.valueDecoders = config.valueDecoders();
        this.provider = config.provider();
        return this;
    }

    /**
     * Set {@link DocumentLoader} used to fetch referenced JSON-LD contexts. If not
     * set then default document loader provided by {@link JsonLdOptions} is used.
     * 
     * @param loader a document loader to set
     * @return {@link Decoder} instance
     */
    public Decoder loader(DocumentLoader loader) {
        this.loader = loader;
        return this;
    }

    /**
     * Use well-known contexts that are bundled with the library instead of fetching
     * it online. <code>true</code> by default. Disabling might cause slower
     * processing.
     *
     * @param enable <code>true</code> to use static bundled contexts
     * @return {@link Decoder} instance
     */
    public Decoder useBundledContexts(boolean enable) {
        this.bundledContexts = enable;
        return this;
    }

    /**
     * If set, then is used as the input document's base IRI.
     *
     * @param base a document base
     * @return {@link Decoder} instance
     */
    public Decoder base(URI base) {
        this.base = base;
        return this;
    }

    /** 
     * Create a new {@link Decoder} instance for the given encoded document.
     * 
     * @param encodedDocument
     * @return
     * @throws DecoderError
     */
    public static final Decoder create(byte[] encodedDocument) throws DecoderError {

        if (encodedDocument == null) {
            throw new IllegalArgumentException("The encoded document paramenter must not be null but byte array.");
        }

        if (encodedDocument.length < 4) {
            throw new DecoderError(Code.InvalidDocument,
                    "The encoded document must be at least 4 bytes but is [" + encodedDocument.length + "].");
        }

        if (encodedDocument[0] != CborLd.CBOR_LD_LEADING_BYTE) {
            throw new DecoderError(Code.InvalidDocument, "The document is not CBOR-LD document. Must start with "
                    + Hex.toString(CborLd.CBOR_LD_LEADING_BYTE)
                    + ", but is "
                    + Hex.toString(encodedDocument[0])
                    + ".");
        }

        if (encodedDocument[1] == CborLd.CBOR_LD_VERSION_5_BYTE) {
            if (encodedDocument[2] == CborLd.COMPRESSED_V1) {
                return new DecoderV5(encodedDocument, true);
            }

            if (encodedDocument[2] == CborLd.UNCOMPRESSED) {
                return new DecoderV5(encodedDocument, false);
            }

            throw new DecoderError(Code.UnknownCompression,
                    "Unkknown CBOR-LD document compression, expected 0x00 - uncompressed or 0x01 - compressed, but found ["
                            + Hex.toString(encodedDocument[2]) + "].");
        }

        if (encodedDocument[1] == CborLd.CBOR_LD_VERSION_6_BYTE) {
            return new DecoderV6(encodedDocument, encodedDocument[2]);
        }

        throw new DecoderError(Code.InvalidDocument, "The document is not CBOR-LD document. Must start with "
                + Hex.toString(CborLd.CBOR_LD_VERSION_6_BYTE)
                + ", or "
                + Hex.toString(CborLd.CBOR_LD_VERSION_5_BYTE)
                + ", but is "
                + Hex.toString(encodedDocument[1])
                + ".");
    }

    /**
     * Decode CBOR-LD document as JSON-LD document.
     * 
     * @return a decoded CBOR-LD document
     *
     * @throws ContextError
     * @throws DecoderError
     */
    public abstract JsonValue decode() throws ContextError, DecoderError;

    protected final JsonValue decodeData(final DataItem data, final String term, final TypeMap def) throws DecoderError, ContextError {

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

        case SPECIAL:
            return decode((Special) data);

        case NEGATIVE_INTEGER:
            return Json.createValue(((NegativeInteger) data).getValue());

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

    protected final JsonObject decodeMap(final Map map, final TypeMap def) throws DecoderError, ContextError {

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
            String term = decodeKey(key);

            if (!isArray && MajorType.ARRAY.equals(value.getMajorType())) {
                json = decodeValue(value, term, def);
            }

            if (json == null) {
                json = decodeData(value, term, def);

                if (isArray
                        && compactArrays
                        && (JsonUtils.isNotArray(json)
                                || json.asJsonArray().size() == 1)) {

                    json = Json.createArrayBuilder().add(json).build();
                }
            }

            builder.add(decodeKey(key), json);
        }

        return builder.build();
    }

    protected final String decodeKey(final DataItem data) {

        if (data == null) {
            throw new IllegalArgumentException("The data parameter must not be null.");
        }

        switch (data.getMajorType()) {
        case UNICODE_STRING:
            return decodeKey(((UnicodeString) data).getString());

        case UNSIGNED_INTEGER:
            return decodeKey(((UnsignedInteger) data).getValue());

        default:
            return data.toString();
        }
    }

    protected final String decodeKey(final String key) {
        return key;
    }

    protected final String decodeKey(final BigInteger key) {

        if (key.mod(BigInteger.ONE.add(BigInteger.ONE)).equals(BigInteger.ZERO)) {
            String result = index.getValue(key);
            return result != null ? result : key.toString();
        }

        String result = index.getValue(key.subtract(BigInteger.ONE));

        return result != null ? result : key.toString();
    }

    protected final JsonArray decodeArray(final Collection<DataItem> items, final String key, final TypeMap def) throws DecoderError, ContextError {

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

    protected final JsonString decodeString(final UnicodeString string, final String key) {

        if (string == null) {
            throw new IllegalArgumentException("The string parameter must not be null.");
        }
        return Json.createValue(string.getString());
    }

    protected final JsonValue decodeInteger(final DataItem number, final String key, final TypeMap def) throws DecoderError {

        if (number == null) {
            throw new IllegalArgumentException("The number parameter must not be null.");
        }

        JsonValue decoded = decodeValue(number, key, def);

        if (decoded != null) {
            return decoded;
        }

        // fallback
        return Json.createValue(((UnsignedInteger) number).getValue());
    }

    protected final JsonValue decodeValue(final DataItem value, final String term, final TypeMap def) throws DecoderError {
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

    protected final JsonValue decode(final Special value) {
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

    protected final JsonValue decode(final SimpleValue value) {
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
    public DictionaryAlgorithm dictonaryAlgorithm() {
        return DictionaryAlgorithm.ProcessingOrderAppliedContexts;
    }

    @Override
    public Collection<ValueDecoder> valueDecoders() {
        return valueDecoders;
    }

    @Override
    public MappingProvider provider() {
        return provider;
    }
}
