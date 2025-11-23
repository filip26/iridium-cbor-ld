package com.apicatalog.cborld.encoder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.net.URI;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.encoder.EncoderException.EncoderCode;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.tree.io.TreeAdapter;

import co.nstant.in.cbor.CborBuilder;
import co.nstant.in.cbor.CborEncoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.builder.ArrayBuilder;
import co.nstant.in.cbor.builder.MapBuilder;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.DoublePrecisionFloat;
import co.nstant.in.cbor.model.NegativeInteger;
import co.nstant.in.cbor.model.SimpleValue;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

public class DefaultEncoder implements Encoder {

    final EncoderConfig config;
    final EncoderMappingProvider mappingProvider;
    final DocumentLoader loader;
    final URI base;

    public DefaultEncoder(EncoderConfig config, EncoderMappingProvider mappingProvider, DocumentLoader loader, URI base) {
        this.config = config;
        this.mappingProvider = mappingProvider;
        this.loader = loader;
        this.base = base;
    }

    /**
     * Encodes JSON-LD document as CBOR-LD document.
     * 
     * @param document JSON-LD document to encode
     * @param adapter
     * @return a byte array representing the encoded CBOR-LD document.
     * 
     * @throws EncoderException
     */
    public final byte[] encode(Object document, TreeAdapter adapter) throws EncoderException {

        try {

            return compress(
                    Objects.requireNonNull(document, "The 'document' parameter must not be null."),
                    Objects.requireNonNull(adapter, "The 'adapter' parameter must not be null."));

            // non compressible context
        } catch (IllegalArgumentException e) {
            /* ignored, expected in a case of non compress-able documents */
        }

        throw new EncoderException(
                EncoderCode.NonCompressible,
                """
                        Non-compressible document. Only JSON-LD documents containing referenced contexts can be compressed. \
                        Referenced contexts serve as a shared dictionary, which is not possible with inline contexts.
                        """);
    }

    /**
     * Compress the given JSON-LD document into CBOR-LD byte array.
     *
     * @param document    the document to compress
     * @param adapter
     * @param contextUrls a set of URLs of <code>@context</code> referenced by the
     *                    document
     * @return the compressed document as byte array
     *
     * @throws IOException
     * @throws EncoderException
     */
    final byte[] compress(final Object document, final TreeAdapter adapter) throws EncoderException {

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();

        try {
            // 2.CBOR Tag - 0xD9
            baos.write(CborLd.LEADING_BYTE);

            final CborBuilder builder = new CborBuilder();

            MapBuilder<?> mapBuilder;

            switch (config.version()) {
            case V1:
                baos.write(config.version().bytes()[0]);
                baos.write(config.version().bytes()[1]);
                mapBuilder = builder.addArray()
                        .add(config.dictionary() != null
                                ? config.dictionary().code()
                                : 0)
                        .addMap();
                break;

            case V06:
            case V05:
                baos.write(config.version().bytes()[0]);
                baos.write(config.dictionary().code());
                mapBuilder = builder.addMap();
                break;

            default:
                throw new EncoderException(EncoderCode.Unsupported, "Unsupported CBOR-LD version " + config.version() + ".");
            }

            // if no compression
            if (config.dictionary() == null) {
                encode(
                        adapter.entries(document),
                        adapter,
                        mapBuilder,
                        null,
                        null)
                        .end();

            } else {
                final Mapping mapping = mappingProvider.getEncoderMapping(document, adapter, this);
                encode(
                        adapter.entries(document),
                        adapter,
                        mapBuilder,
                        mapping.typeMap(),
                        mapping)
                        .end();
            }

            new CborEncoder(baos).encode(builder.build());

            return baos.toByteArray();

        } catch (CborException e) {
            throw new EncoderException(EncoderCode.InvalidDocument, e);

        } catch (JsonLdException e) {
            throw new EncoderException(EncoderCode.Internal, e);
        }
    }

    final MapBuilder<?> encode(
            final Iterable<Entry<?, ?>> entries,
            final TreeAdapter adapter,
            final MapBuilder<?> builder,
            final TypeMap typeMapping,
            final Mapping mapping) throws EncoderException, JsonLdException {

        MapBuilder<?> flow = builder;

        for (final Entry<?, ?> entry : entries) {

            final String property = adapter.stringValue(entry.getKey());

            final Integer encodedProperty = mapping != null
                    ? mapping.termMap().getCode(property)
                    : null;

            if (adapter.isCollection(entry.getValue())) {

                var key = encodedProperty != null
                        ? new UnsignedInteger(encodedProperty + 1)
                        : new UnicodeString(property);

                var entryValue = entry.getValue();

                if (config.isCompactArrays() && adapter.isSingleElement(entryValue)) {

                    var singleton = adapter.singleElement(entryValue);

                    if (adapter.isMap(singleton)) {
                        final TypeMap propertyTypeMapping = typeMapping.getMapping(property);
                        flow = (MapBuilder<?>) encode(adapter.entries(singleton), adapter, flow.putMap(key), propertyTypeMapping, mapping).end();

                    } else if (adapter.isCollection(singleton)) {
                        flow = (MapBuilder<?>) encode(
                                adapter.elements(singleton),
                                adapter,
                                flow.putArray(key),
                                property,
                                typeMapping,
                                mapping).end();

                    } else {
                        flow = flow.put(key, encode(singleton, adapter, property, typeMapping, mapping));
                    }

                } else {
                    flow = (MapBuilder<?>) encode(
                            adapter.elements(entryValue),
                            adapter,
                            flow.putArray(key),
                            property,
                            typeMapping, mapping).end();
                }
                continue;
            }

            final DataItem key = encodedProperty != null
                    ? new UnsignedInteger(encodedProperty)
                    : new UnicodeString(property);

            if (adapter.isMap(entry.getValue())) {

                final TypeMap propertyTypeMapping = typeMapping != null
                        ? typeMapping.getMapping(property)
                        : null;

                flow = (MapBuilder<?>) encode(
                        adapter.entries(entry.getValue()),
                        adapter,
                        flow.putMap(key),
                        propertyTypeMapping,
                        mapping).end();

                continue;
            }

            flow = flow.put(
                    key,
                    encode(entry.getValue(), adapter, property, typeMapping, mapping));
        }

        return flow;
    }

    final DataItem encode(final Object jsonValue, final TreeAdapter adapter, final String term, TypeMap typeMapping, Mapping mapping) throws EncoderException {

        if (jsonValue == null) {
            return SimpleValue.NULL;
        }

        switch (adapter.type(jsonValue)) {
        case NULL:
            return SimpleValue.NULL;

        case TRUE:
            return SimpleValue.TRUE;

        case FALSE:
            return SimpleValue.FALSE;

        case STRING:
            final var types = Optional
                    .ofNullable(typeMapping)
                    .map(tm -> tm.getType(term))
                    .orElse(null);

            final var stringValue = adapter.stringValue(jsonValue);

            for (final var valueEncoder : config.valueEncoders()) {
                final var dataItem = valueEncoder.encode(mapping, stringValue, term, types);
                if (dataItem != null) {
                    return dataItem;
                }
            }
            return new UnicodeString(stringValue);

        case NUMBER:
            if (adapter.isIntegral(jsonValue)) {

                var integer = adapter.integerValue(jsonValue);

                switch (integer.signum()) {
                case -1:
                    return new NegativeInteger(integer);
                case 0:
                    return new UnsignedInteger(BigInteger.ZERO);
                case 1:
                    return new UnsignedInteger(integer);
                }

            } else {
                // then it's decimal
                return new DoublePrecisionFloat(adapter.doubleValue(jsonValue));
            }

        default:
            throw new IllegalStateException();
        }
    }

    final ArrayBuilder<?> encode(
            final Iterable<?> jsonArray,
            final TreeAdapter adapter,
            final ArrayBuilder<?> builder,
            String property,
            TypeMap typeMapping,
            Mapping mapping)
            throws EncoderException, JsonLdException {

        var flow = builder;

        for (var item : adapter.elements(jsonArray)) {

            if (adapter.isMap(item)) {
                flow = (ArrayBuilder<?>) encode(
                        adapter.entries(item),
                        adapter,
                        flow.startMap(),
                        typeMapping,
                        mapping)
                        .end();
                continue;
            }

            if (adapter.isCollection(item)) {
                flow = (ArrayBuilder<?>) encode(
                        adapter.elements(item),
                        adapter,
                        flow.startArray(),
                        property,
                        typeMapping,
                        mapping)
                        .end();
                continue;
            }

            flow = flow.add(encode(item, adapter, property, typeMapping, mapping));
        }

        return flow;
    }

    public URI base() {
        return base;
    }

    public DocumentLoader loader() {
        return loader;
    }

    public EncoderConfig config() {
        return config;
    }
}
