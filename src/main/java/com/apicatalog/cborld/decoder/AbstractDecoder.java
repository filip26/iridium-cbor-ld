package com.apicatalog.cborld.decoder;

import java.io.ByteArrayInputStream;
import java.math.BigInteger;
import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderException.Code;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
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

abstract class AbstractDecoder implements Decoder {

    static final byte UNCOMPRESSED_BYTE = 0x00;

    static final byte COMPRESSED_BYTE = 0x01;

    final DecoderMappingProvider mappingProvider;
    
    final DecoderConfig config;
    final DocumentLoader loader;
    final URI base;

    protected AbstractDecoder(DecoderConfig config, DecoderMappingProvider mappingProvider, DocumentLoader loader, URI base) {
        this.config = config;
        this.mappingProvider = mappingProvider;
        this.loader = loader;
        this.base = base;
    }

    @Override
    public JsonValue decode(byte[] encoded) throws ContextError, DecoderException {
        final CborLdVersion version = Decoder.assertCborLd(encoded);

        if (version != config.version()) {
            throw new DecoderException(Code.Unsupported, "The decoder does support " + version + " but " + config.version() + " .");

        }
        return decode(version, encoded);
    }

    protected final JsonValue decode(final DocumentDictionary dictionary, final DataItem data) throws DecoderException, ContextError {
        final Mapping mapping = mappingProvider.getDecoderMapping(data, dictionary, this);
        return decodeData(data, null, mapping.typeMap(), mapping);
    }

    protected final JsonValue decodeData(final DataItem data, final String term, final TypeMap def, Mapping mapping) throws DecoderException, ContextError {

        Objects.requireNonNull(data);

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
                throw new DecoderException(Code.InvalidDocument, "Unknown encoded value [" + data.getMajorType() + "] at key [" + term + "].");
            }

            return decoded;

        default:
            throw new IllegalStateException("An unexpected data item type [" + data.getMajorType() + "].");
        }
    }

    protected final JsonObject decodeMap(final co.nstant.in.cbor.model.Map map, final TypeMap def, final Mapping mapping) throws DecoderException, ContextError {

        Objects.requireNonNull(map);

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

        Objects.requireNonNull(data);

        switch (data.getMajorType()) {
        case UNICODE_STRING:
            return ((UnicodeString) data).getString();

        case UNSIGNED_INTEGER:
            return decodeKey(((UnsignedInteger) data).getValue(), mapping);

        default:
            return data.toString();
        }
    }

    protected static final String decodeKey(final BigInteger key, final Mapping mapping) {

        final String result = key.mod(BigInteger.ONE.add(BigInteger.ONE)).equals(BigInteger.ZERO)
                ? mapping.termMap().getValue(key.intValueExact())
                : mapping.termMap().getValue(key.subtract(BigInteger.ONE).intValueExact());

        return result != null ? result : key.toString();
    }

    protected final JsonArray decodeArray(final Collection<DataItem> items, final String key, final TypeMap def, final Mapping mapping) throws DecoderException, ContextError {

        Objects.requireNonNull(items);

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
        Objects.requireNonNull(string);
        return Json.createValue(string.getString());
    }

    protected final JsonValue decodeInteger(final DataItem number, final String key, final TypeMap def, final Mapping mapping) throws DecoderException {

        Objects.requireNonNull(number);

        JsonValue decoded = decodeValue(number, key, def, mapping);

        if (decoded != null) {
            return decoded;
        }

        // fallback
        return Json.createValue(((UnsignedInteger) number).getValue());
    }

    protected final JsonValue decodeValue(final DataItem value, final String property, final TypeMap def, final Mapping mapping) throws DecoderException {
        if (def != null) {
            final Collection<String> types = def.getType(property);

            for (final ValueDecoder decoder : config.valueDecoders()) {
                var decoded = decoder.decode(mapping, value, property, types);

                if (decoded != null) {
                    return Json.createValue(decoded);
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


    // legacy support
    protected final JsonValue decode(final DocumentDictionary dictionary, byte[] encoded) throws ContextError, DecoderException {
        try {
            final ByteArrayInputStream bais = new ByteArrayInputStream(encoded);
            final List<DataItem> dataItems = new CborDecoder(bais).decode();

            // nothing do de-compress
            if (dataItems.isEmpty()) {
                return null;
            }

            // only one object
            if (dataItems.size() == 1) {
                return decode(dictionary, dataItems.iterator().next());
            }

            // decode as an array of objects
            final JsonArrayBuilder builder = Json.createArrayBuilder();

            for (final DataItem item : dataItems) {
                builder.add(decode(dictionary, item));
            }

            return builder.build();

        } catch (final CborException e) {
            throw new DecoderException(Code.InvalidDocument, e);
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
