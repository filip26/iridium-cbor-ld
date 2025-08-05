package com.apicatalog.cborld.decoder;

import java.io.ByteArrayInputStream;
import java.math.BigInteger;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.loader.DocumentLoader;

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

public abstract class BaseDecoder implements Decoder {

    protected final DecoderConfig config;

    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected URI base;

    protected BaseDecoder(DecoderConfig config, DocumentLoader loader, URI base) {
        this.config = config;
        this.loader = loader;
        this.base = base;
    }

    @Override
    public JsonValue decode(byte[] encoded) throws ContextError, DecoderError {
        final CborLdVersion version = BaseDecoder.assertCborLd(encoded);

        if (version != config.version()) {
            throw new DecoderError(Code.Unsupported, "The decoder does support " + version + " but " + config.version() + " .");

        }
        return decode(version, encoded);
    }

    protected final JsonValue decode(final DataItem data, final DocumentDictionary custom) throws DecoderError, ContextError {
        final Mapping mapping = config.decoderMapping().getDecoderMapping(data, custom, this);
        return decodeData(data, null, mapping.typeMap(), mapping);
    }

    protected final JsonValue decodeData(final DataItem data, final String term, final TypeMap def, Mapping mapping) throws DecoderError, ContextError {

        if (data == null) {
            throw new IllegalArgumentException("The data parameter must not be null.");
        }

        switch (data.getMajorType()) {
        case MAP:
            return decodeMap((co.nstant.in.cbor.model.Map) data, term != null ? def.getMapping(term) : def, mapping);

        case ARRAY:
            return decodeArray(((Array) data).getDataItems(), term, def, mapping);

        case UNICODE_STRING:
            return decodeString((UnicodeString) data);

        case UNSIGNED_INTEGER:
            return decodeInteger(data, term, def, mapping);

        case SPECIAL:
            return decode((Special) data);

        case NEGATIVE_INTEGER:
            return Json.createValue(((NegativeInteger) data).getValue());

        case BYTE_STRING:
            JsonValue decoded = decodeValue(data, term, def, mapping);
            if (decoded == null) {
                throw new DecoderError(Code.InvalidDocument, "Unknown encoded value [" + data.getMajorType() + "] at key [" + term + "].");
            }

            return decoded;

        default:
            throw new IllegalStateException("An unexpected data item type [" + data.getMajorType() + "].");
        }
    }

    protected final JsonObject decodeMap(final co.nstant.in.cbor.model.Map map, final TypeMap def, final Mapping mapping) throws DecoderError, ContextError {

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
            final String term = decodeKey(key, mapping);

            if (!isArray && MajorType.ARRAY.equals(value.getMajorType())) {
                json = decodeValue(value, term, def, mapping);
            }

            if (json == null) {
                json = decodeData(value, term, def, mapping);

                if (isArray
                        && config.isCompactArrays()
                        && (JsonUtils.isNotArray(json)
                                || json.asJsonArray().size() == 1)) {

                    json = Json.createArrayBuilder().add(json).build();
                }
            }

            builder.add(term, json);
        }

        return builder.build();
    }

    protected static final String decodeKey(final DataItem data, final Mapping mapping) {

        if (data == null) {
            throw new IllegalArgumentException("The data parameter must not be null.");
        }

        switch (data.getMajorType()) {
        case UNICODE_STRING:
            return decodeKey(((UnicodeString) data).getString());

        case UNSIGNED_INTEGER:
            return decodeKey(((UnsignedInteger) data).getValue(), mapping);

        default:
            return data.toString();
        }
    }

    protected static final String decodeKey(final String key) {
        return key;
    }

    protected static final String decodeKey(final BigInteger key, final Mapping mapping) {

        final String result = key.mod(BigInteger.ONE.add(BigInteger.ONE)).equals(BigInteger.ZERO)
                ? mapping.terms().getValue(key.intValueExact())
                : mapping.terms().getValue(key.subtract(BigInteger.ONE).intValueExact());

        return result != null ? result : key.toString();
    }

    protected final JsonArray decodeArray(final Collection<DataItem> items, final String key, final TypeMap def, final Mapping mapping) throws DecoderError, ContextError {

        if (items == null) {
            throw new IllegalArgumentException("The items parameter must not be null.");
        }

        if (items.isEmpty()) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }

        final JsonArrayBuilder builder = Json.createArrayBuilder();

        for (final DataItem item : items) {
            builder.add(decodeData(item, key, def, mapping));
        }

        return builder.build();
    }

    protected static final JsonString decodeString(final UnicodeString string) {

        if (string == null) {
            throw new IllegalArgumentException("The string parameter must not be null.");
        }
        return Json.createValue(string.getString());
    }

    protected final JsonValue decodeInteger(final DataItem number, final String key, final TypeMap def, final Mapping mapping) throws DecoderError {

        if (number == null) {
            throw new IllegalArgumentException("The number parameter must not be null.");
        }

        JsonValue decoded = decodeValue(number, key, def, mapping);

        if (decoded != null) {
            return decoded;
        }

        // fallback
        return Json.createValue(((UnsignedInteger) number).getValue());
    }

    protected final JsonValue decodeValue(final DataItem value, final String term, final TypeMap def, final Mapping mapping) throws DecoderError {
        if (def != null) {
            final Collection<String> types = def.getType(term);

            for (final ValueDecoder decoder : config.valueDecoders()) {
                final JsonValue decoded = decoder.decode(mapping, value, term, types);

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

    protected static final CborLdVersion assertCborLd(byte[] encoded) throws DecoderError {
        if (encoded == null) {
            throw new IllegalArgumentException("The encoded document paramenter must not be null but byte array.");
        }

        if (encoded.length < 4) {
            throw new DecoderError(Code.InvalidDocument,
                    "The encoded document must be at least 4 bytes but is [" + encoded.length + "].");
        }

        if (encoded[0] != CborLd.LEADING_BYTE) {
            throw new DecoderError(Code.InvalidDocument, "The document is not CBOR-LD document. Must start with "
                    + Hex.toString(CborLd.LEADING_BYTE)
                    + ", but is "
                    + Hex.toString(encoded[0])
                    + ".");
        }

        final CborLdVersion version = CborLdVersion.of(encoded, 1); // skip leading byte

        if (version == null) {
            throw new DecoderError(Code.InvalidDocument, "The document is not CBOR-LD document. A tag must start with: "
                    + "v1.0 = "
                    + Hex.toString(CborLd.VERSION_10_BYTES)
                    + ", or v0.6 = "
                    + Hex.toString(new byte[] { CborLd.VERSION_06_BYTE })
                    + ", or v0.5 = "
                    + Hex.toString(new byte[] { CborLd.VERSION_05_BYTE, CborLd.COMPRESSED_BYTE })
                    + ", but is "
                    + Hex.toString(Arrays.copyOfRange(encoded, 1, 3))
                    + ".");
        }

        return version;
    }

    // legacy support
    protected JsonValue decode(byte[] encoded, final DocumentDictionary provider) throws ContextError, DecoderError {
        try {
            final ByteArrayInputStream bais = new ByteArrayInputStream(encoded);
            final List<DataItem> dataItems = new CborDecoder(bais).decode();

            // nothing do de-compress
            if (dataItems.isEmpty()) {
                return null;
            }

            // only one object
            if (dataItems.size() == 1) {
                return decode(dataItems.iterator().next(), provider);
            }

            // decode as an array of objects
            final JsonArrayBuilder builder = Json.createArrayBuilder();

            for (final DataItem item : dataItems) {
                builder.add(decode(item, provider));
            }

            return builder.build();

        } catch (final CborException e) {
            throw new DecoderError(Code.InvalidDocument, e);
        }
    }

    @Override
    public DecoderConfig config() {
        return config;
    }

    @Override
    public URI base() {
        return base;
    }

    @Override
    public DocumentLoader loader() {
        return loader;
    }
}
