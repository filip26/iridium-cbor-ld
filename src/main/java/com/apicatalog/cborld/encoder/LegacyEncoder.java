package com.apicatalog.cborld.encoder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.net.URI;
import java.util.Collection;
import java.util.Map.Entry;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.encoder.EncoderError.Code;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
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

public class LegacyEncoder implements Encoder {

    protected final EncoderConfig config;
    protected DocumentLoader loader;
    protected URI base;

    protected LegacyEncoder(EncoderConfig config, DocumentLoader loader, URI base) {
        this.config = config;
        this.loader = loader;
        this.base = base;
    }

    /**
     * Encodes JSON-LD document as CBOR-LD document.
     * 
     * @param document JSON-LD document to encode
     * @return a byte array representing the encoded CBOR-LD document.
     * 
     * @throws EncoderError
     * @throws ContextError
     */
    public final byte[] encode(JsonObject document) throws EncoderError, ContextError {

        if (document == null) {
            throw new IllegalArgumentException("The 'document' parameter must not be null.");
        }

//        return encode(JakartaJsonCursor.from(document));
//    }
//
//    /**
//     * Encode JSON-LD document as CBOR-LD document.
//     * 
//     * @return a byte array representing the encoded CBOR-LD document.
//     * 
//     * @throws EncoderError
//     * @throws ContextError
//     */
//    byte[] encode(MapCursor document) throws EncoderError, ContextError {

        try {

            final Collection<String> contexts = EncoderContext.get(document);

            if (contexts.isEmpty()) { // is not JSON-LD document
                throw new EncoderError(Code.InvalidDocument, "Not a valid JSON-LD document in a compacted form. @context declaration is missing");
            }

            return compress(document, contexts);

            // non compressable context
        } catch (IllegalArgumentException e) {
            /* ignored, expected in a case of non compress-able documents */
        }

        throw new EncoderError(Code.InvalidDocument, "Non compress-able document.");
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
     * @throws EncoderError
     */
    final byte[] compress(final JsonObject document, Collection<String> contextUrls) throws ContextError, EncoderError {

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();

        try {
            final Mapping mapping = config.encoderMapping().getEncoderMapping(document, this);

            CborBuilder builder = (CborBuilder) encode(document, new CborBuilder().addMap(), mapping.typeMap(), mapping).end();

            // 2.CBOR Tag - 0xD9
            baos.write(CborLd.LEADING_BYTE);
            
            switch (config.version()) {
            case V1:
                baos.write(CborLd.VERSION_10_BYTES[0]);
                baos.write(CborLd.VERSION_10_BYTES[1]);                
                break;

            case V06:
                baos.write(CborLd.VERSION_06_BYTE);
                baos.write(config.dictionary().code());
                break;
                
            case V05:
                baos.write(CborLd.VERSION_05_BYTE);
                baos.write(config.dictionary().code());
                break;
            default:
                throw new EncoderError(Code.Unsupported, "Unsupported CBOR-LD version " + config.version() + ".");
            }

            new CborEncoder(baos).encode(builder.build());

            return baos.toByteArray();

        } catch (CborException e) {
            throw new EncoderError(Code.InvalidDocument, e);

        } catch (JsonLdError e) {
            throw new EncoderError(Code.Internal, e);
        }
    }

    final MapBuilder<?> encode(final JsonObject object, final MapBuilder<?> builder, TypeMap typeMapping, Mapping mapping) throws EncoderError, JsonLdError {

        if (object.isEmpty()) {
            return builder;
        }

        MapBuilder<?> flow = builder;

        for (final Entry<String, JsonValue> entry : object.entrySet()) {
            final String property = entry.getKey();

            final Integer encodedProperty = mapping.terms().getCode(property);

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
                final TypeMap propertyTypeMapping = typeMapping.getMapping(property);
                flow = (MapBuilder<?>) encode(entry.getValue().asJsonObject(), flow.putMap(key),
                        propertyTypeMapping, mapping).end();
                continue;
            }

            final DataItem value = encode(entry.getValue(), property, typeMapping, mapping);

            flow = flow.put(key, value);
        }

        return flow;
    }

    final DataItem encode(final JsonValue jsonValue, final String term, TypeMap typeMapping, Mapping mapping) throws EncoderError {

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
            if (typeMapping != null) {
                final Collection<String> types = typeMapping.getType(term);

                for (final ValueEncoder valueEncoder : config.valueEncoders()) {
                    final DataItem dataItem = valueEncoder.encode(mapping, jsonValue, term, types);
                    if (dataItem != null) {
                        return dataItem;
                    }
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

    final ArrayBuilder<?> encode(final JsonArray jsonArray, final ArrayBuilder<?> builder, String property, TypeMap typeMapping, Mapping mapping) throws EncoderError, JsonLdError {

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
