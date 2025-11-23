package com.apicatalog.cborld.decoder;

import java.io.ByteArrayInputStream;
import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.apicatalog.cborld.CborLd.Version;
import com.apicatalog.cborld.decoder.DecoderException.DecoderCode;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.DoublePrecisionFloat;
import co.nstant.in.cbor.model.HalfPrecisionFloat;
import co.nstant.in.cbor.model.NegativeInteger;
import co.nstant.in.cbor.model.SimpleValue;
import co.nstant.in.cbor.model.SinglePrecisionFloat;
import co.nstant.in.cbor.model.Special;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

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
    public Object decode(byte[] encoded) throws DecoderException {
        final Version version = Decoder.assertCborLd(encoded);

        if (version != config.version()) {
            throw new DecoderException(DecoderCode.Unsupported, "The decoder does support " + version + " but " + config.version() + " .");

        }
        return decode(version, encoded);
    }

    protected final Object decode(final DocumentDictionary dictionary, final DataItem data) throws DecoderException {
        final Mapping mapping = mappingProvider.getDecoderMapping(data, dictionary, this);
        return decodeData(data, null, mapping.typeMap(), mapping);
    }

    protected final Object decodeData(final DataItem data, final String term, final TypeMap def, Mapping mapping) throws DecoderException {

        Objects.requireNonNull(data);

        switch (data.getMajorType()) {
        case MAP:
            return decodeMap((co.nstant.in.cbor.model.Map) data, term != null && def != null ? def.getMapping(term) : def, mapping);

        case ARRAY:
            return decodeArray(((Array) data).getDataItems(), term, def, mapping);

        case UNICODE_STRING:
            return decodeString((UnicodeString) data);

        case UNSIGNED_INTEGER:
            return decodeInteger(data, term, def, mapping);

        case SPECIAL:
            return decode((Special) data);

        case NEGATIVE_INTEGER:
            return ((NegativeInteger) data).getValue();

        case BYTE_STRING:
            String decoded = decodeValue(data, term, def, mapping);
            if (decoded != null) {
                return decoded;
            }

        default:
            throw new IllegalStateException("An unexpected data item type " + data + " at " + term + ".");
        }
    }

    protected final Map<String, Object> decodeMap(final co.nstant.in.cbor.model.Map map, final TypeMap def, final Mapping mapping) throws DecoderException {

        Objects.requireNonNull(map);

        if (map.getKeys().isEmpty()) {
            return Collections.emptyMap();
        }

        var result = new LinkedHashMap<String, Object>();

        for (var key : map.getKeys()) {

            final DataItem value = map.get(key);

            boolean isArray = key instanceof UnsignedInteger uint
                    && !uint.getValue().mod(BigInteger.ONE.add(BigInteger.ONE)).equals(BigInteger.ZERO);

            var json = (Object) null;

            final String term = decodeKey(key, mapping);

            if (!isArray && value instanceof Array) {
                json = decodeValue(value, term, def, mapping);
            }

            if (json == null) {
                json = decodeData(value, term, def, mapping);

                if (isArray
                        && config.isCompactArrays()
                        && (!(json instanceof Collection c)
                                || c.size() == 1)) {

                    json = Set.of(json);
                }
            }

            result.put(term, json);
        }

        return result;
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

    protected final Collection<Object> decodeArray(final Collection<DataItem> items, final String key, final TypeMap def, final Mapping mapping) throws DecoderException {

        Objects.requireNonNull(items);

        if (items.isEmpty()) {
            return Collections.emptyList();
        }

        var result = new ArrayList<Object>(items.size());

        for (final DataItem item : items) {
            result.add(decodeData(item, key, def, mapping));
        }

        return result;
    }

    protected static final String decodeString(final UnicodeString string) {
        Objects.requireNonNull(string);
        return string.getString();
    }

    protected final Object decodeInteger(final DataItem number, final String key, final TypeMap def, final Mapping mapping) throws DecoderException {

        Objects.requireNonNull(number);

        String decoded = decodeValue(number, key, def, mapping);

        if (decoded != null) {
            return decoded;
        }

        // fallback
        return ((UnsignedInteger) number).getValue();
    }

    protected final String decodeValue(final DataItem value, final String property, final TypeMap def, final Mapping mapping) throws DecoderException {

        final var type = def != null
                ? def.getType(property)
                : null;

        for (final ValueDecoder decoder : config.valueDecoders()) {
            var decoded = decoder.decode(mapping, value, property, type);

            if (decoded != null) {
                return decoded;
            }
        }
        return null;
    }

    protected static final Object decode(final Special value) {
        switch (value.getSpecialType()) {
        case IEEE_754_DOUBLE_PRECISION_FLOAT:
            return ((DoublePrecisionFloat) value).getValue();

        case IEEE_754_HALF_PRECISION_FLOAT:
            return ((HalfPrecisionFloat) value).getValue();

        case IEEE_754_SINGLE_PRECISION_FLOAT:
            return ((SinglePrecisionFloat) value).getValue();

        case SIMPLE_VALUE:
            return decode((SimpleValue) value);

        default:
            break;
        }

        throw new IllegalArgumentException("Unsupported CBOR special type [" + value.getSpecialType() + "].");
    }

    protected static final Object decode(final SimpleValue value) {
        switch (value.getSimpleValueType()) {
        case FALSE:
            return false;

        case TRUE:
            return true;

        case NULL:
            return null;

        default:
            break;
        }

        throw new IllegalArgumentException("Unsupported CBOR simple value type [" + value.getSimpleValueType() + "].");
    }

    // legacy support
    protected final Object decode(final DocumentDictionary dictionary, byte[] encoded) throws DecoderException {
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
            var list = new ArrayList<Object>(dataItems.size());

            for (final DataItem item : dataItems) {
                list.add(decode(dictionary, item));
            }

            return list;

        } catch (final CborException e) {
            throw new DecoderException(DecoderCode.InvalidDocument, e);
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
