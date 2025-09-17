package com.apicatalog.cborld.encoder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.Map.Entry;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.encoder.EncoderException.Code;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.loader.DocumentLoader;

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
import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

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
     * @return a byte array representing the encoded CBOR-LD document.
     * 
     * @throws EncoderException
     * @throws ContextError
     */
    public final byte[] encode(JsonObject document) throws EncoderException, ContextError {

        if (document == null) {
            throw new IllegalArgumentException("The 'document' parameter must not be null.");
        }

        try {

            final Collection<String> contexts = EncoderContext.get(document);

            if (contexts.isEmpty()) { // is not JSON-LD document
                throw new EncoderException(Code.InvalidDocument, "Not a valid JSON-LD document in a compacted form. @context declaration is missing");
            }

            return compress(document, contexts);

            // non compressible context
        } catch (IllegalArgumentException e) {
            /* ignored, expected in a case of non compress-able documents */
        }

        throw new EncoderException(
                Code.NonCompressible,
                """
                        Non-compressible document. Only JSON-LD documents containing referenced contexts can be compressed. \
                        Referenced contexts serve as a shared dictionary, which is not possible with inline contexts.
                        """);
    }

    /**
     * Compress the given JSON-LD document into CBOR-LD byte array.
     *
     * @param document    the document to compress
     * @param contextUrls a set of URLs of <code>@context</code> referenced by the
     *                    document
     * @return the compressed document as byte array
     *
     * @throws IOException
     * @throws ContextError
     * @throws EncoderException
     */
    final byte[] compress(final JsonObject document, Collection<String> contextUrls) throws ContextError, EncoderException {

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
                throw new EncoderException(Code.Unsupported, "Unsupported CBOR-LD version " + config.version() + ".");
            }

            // if no compression
            if (config.dictionary() == null) {
                encode(document, mapBuilder, null, null).end();

            } else {
                final Mapping mapping = mappingProvider.getEncoderMapping(document, this);
                encode(document, mapBuilder, mapping.typeMap(), mapping).end();
            }

            new CborEncoder(baos).encode(builder.build());

            return baos.toByteArray();

        } catch (CborException e) {
            throw new EncoderException(Code.InvalidDocument, e);

        } catch (JsonLdError e) {
            throw new EncoderException(Code.Internal, e);
        }
    }

    final MapBuilder<?> encode(final JsonObject object, final MapBuilder<?> builder, TypeMap typeMapping, Mapping mapping) throws EncoderException, JsonLdError {

        if (object.isEmpty()) {
            return builder;
        }

        MapBuilder<?> flow = builder;

        for (final Entry<String, JsonValue> entry : object.entrySet()) {
            final String property = entry.getKey();

            final Integer encodedProperty = mapping != null
                    ? mapping.termMap().getCode(property)
                    : null;

            if (JsonUtils.isArray(entry.getValue())) {

                final DataItem key = encodedProperty != null
                        ? new UnsignedInteger(encodedProperty + 1)
                        : new UnicodeString(property);

                if (config.isCompactArrays() && entry.getValue().asJsonArray().size() == 1) {

                    final JsonValue value = entry.getValue().asJsonArray().iterator().next();

                    if (JsonUtils.isObject(value)) {
                        final TypeMap propertyTypeMapping = typeMapping.getMapping(property);
                        flow = (MapBuilder<?>) encode(value.asJsonObject(), flow.putMap(key), propertyTypeMapping, mapping).end();

                    } else if (JsonUtils.isArray(value)) {
                        flow = (MapBuilder<?>) encode(
                                value.asJsonArray(),
                                flow.putArray(key),
                                property,
                                typeMapping,
                                mapping).end();

                    } else {
                        final DataItem dataItem = encode(value, property, typeMapping, mapping);

                        flow = flow.put(key, dataItem);
                    }

                } else {
                    flow = (MapBuilder<?>) encode(entry.getValue().asJsonArray(), flow.putArray(key),
                            property,
                            typeMapping, mapping).end();
                }
                continue;
            }

            final DataItem key = encodedProperty != null
                    ? new UnsignedInteger(encodedProperty)
                    : new UnicodeString(property);

            if (JsonUtils.isObject(entry.getValue())) {
                final TypeMap propertyTypeMapping = typeMapping != null
                        ? typeMapping.getMapping(property)
                        : null;
                flow = (MapBuilder<?>) encode(entry.getValue().asJsonObject(), flow.putMap(key),
                        propertyTypeMapping, mapping).end();
                continue;
            }

            final DataItem value = encode(entry.getValue(), property, typeMapping, mapping);

            flow = flow.put(key, value);
        }

        return flow;
    }

    final DataItem encode(final JsonValue jsonValue, final String term, TypeMap typeMapping, Mapping mapping) throws EncoderException {

        if (JsonUtils.isNull(jsonValue)) {
            return SimpleValue.NULL;
        }

        if (JsonUtils.isTrue(jsonValue)) {
            return SimpleValue.TRUE;
        }

        if (JsonUtils.isFalse(jsonValue)) {
            return SimpleValue.FALSE;
        }

        if (JsonUtils.isString(jsonValue)) {
            final Collection<String> types = typeMapping != null
                    ? typeMapping.getType(term)
                    : Collections.emptySet();

            for (final ValueEncoder valueEncoder : config.valueEncoders()) {
                final DataItem dataItem = valueEncoder.encode(mapping, ((JsonString) jsonValue).getString(), term, types);
                if (dataItem != null) {
                    return dataItem;
                }
            }
            return new UnicodeString(((JsonString) jsonValue).getString());
        }

        if (JsonUtils.isNumber(jsonValue)) {

            final JsonNumber jsonNumber = ((JsonNumber) jsonValue);

            if (jsonNumber.isIntegral()) {

                BigInteger integer = jsonNumber.bigIntegerValueExact();

                switch (integer.signum()) {
                case -1:
                    return new NegativeInteger(integer);
                case 0:
                    return new UnsignedInteger(BigInteger.ZERO);
                case 1:
                    return new UnsignedInteger(integer);
                }

                // then it's decimal
            } else {
                return new DoublePrecisionFloat(jsonNumber.bigDecimalValue().doubleValue());
            }
        }

        throw new IllegalStateException();
    }

    final ArrayBuilder<?> encode(final JsonArray jsonArray, final ArrayBuilder<?> builder, String property, TypeMap typeMapping, Mapping mapping) throws EncoderException, JsonLdError {

        if (jsonArray.isEmpty()) {
            return builder;
        }

        ArrayBuilder<?> flow = builder;

        for (final JsonValue item : jsonArray) {

            if (JsonUtils.isObject(item)) {
                flow = (ArrayBuilder<?>) encode(item.asJsonObject(), flow.startMap(), typeMapping, mapping).end();
                continue;
            }

            if (JsonUtils.isArray(item)) {
                flow = (ArrayBuilder<?>) encode(item.asJsonArray(), flow.startArray(), property, typeMapping, mapping).end();
                continue;
            }

            final DataItem value = encode(item, property, typeMapping, mapping);

            flow = flow.add(value);
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
