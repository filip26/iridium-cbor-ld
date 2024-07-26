package com.apicatalog.cborld.encoder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Collection;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.encoder.EncoderError.Code;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.ArrayItemCursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.MapEntryCursor;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.cursor.jakarta.JakartaJsonCursor;
import com.apicatalog.jsonld.JsonLdError;

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
import jakarta.json.JsonObject;

public class Encoder {

    protected final EncoderConfig config;

    protected Encoder(EncoderConfig config) {
        this.config = config;
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

        return encode(JakartaJsonCursor.from(document));
    }

    /**
     * Encode JSON-LD document as CBOR-LD document.
     * 
     * @return a byte array representing the encoded CBOR-LD document.
     * 
     * @throws EncoderError
     * @throws ContextError
     */
    byte[] encode(MapCursor document) throws EncoderError, ContextError {

        try {

            final Collection<String> contexts = EncoderContext.get(document);

            if (contexts.isEmpty()) { // is not JSON-LD document
                throw new EncoderError(Code.InvalidDocument, "Not a valid JSON-LD document in a compacted form.");
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
    final byte[] compress(final MapCursor document, Collection<String> contextUrls) throws ContextError, EncoderError {

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();

        try {
            final Mapping mapping = config.encoderMapping().getEncoderMapping(document, config);

            final CborBuilder builder = (CborBuilder) encode(document, new CborBuilder().addMap(), mapping.typeMap(), mapping).end();

            // 2.CBOR Tag - 0xD9, CBOR-LD - version, Compressed - CBOR-LD compression
            // algorithm
            baos.write(CborLd.LEADING_BYTE);
            baos.write(config.version());
            baos.write(config.dictionary().code());

            new CborEncoder(baos).encode(builder.build());

            return baos.toByteArray();

        } catch (CborException e) {
            throw new EncoderError(Code.InvalidDocument, e);

        } catch (JsonLdError e) {
            throw new EncoderError(Code.Internal, e);
        }
    }

    final MapBuilder<?> encode(final MapCursor object, final MapBuilder<?> builder, TypeMap typeMapping, Mapping mapping) throws EncoderError, JsonLdError {

        if (object.isEmpty()) {
            return builder;
        }

        MapBuilder<?> flow = builder;

        for (final MapEntryCursor entry : object) {

            final String property = entry.mapKey();

            final Integer encodedProperty = mapping.terms().getCode(property);

            if (entry.isArray()) {

                final DataItem key = encodedProperty != null
                        ? new UnsignedInteger(encodedProperty + 1)
                        : new UnicodeString(property);

                if (config.isCompactArrays() && object.asArray().size() == 1) {

                    object.asArray().item(0);

                    if (object.isMap()) {
                        final TypeMap propertyTypeMapping = typeMapping.getMapping(property);
                        flow = (MapBuilder<?>) encode(object.asMap(), flow.putMap(key), propertyTypeMapping, mapping).end();

                    } else if (object.isArray()) {
                        flow = (MapBuilder<?>) encode(
                                object.asArray(),
                                flow.putArray(key),
                                property,
                                typeMapping,
                                mapping).end();

                    } else {
                        final DataItem value = encode(object, property, typeMapping, mapping);

                        flow = flow.put(key, value);
                    }

                    object.parent();

                } else {
                    flow = (MapBuilder<?>) encode(object.asArray(), flow.putArray(key),
                            property,
                            typeMapping, mapping).end();
                }
                continue;
            }

            final DataItem key = encodedProperty != null
                    ? new UnsignedInteger(encodedProperty)
                    : new UnicodeString(property);

            if (entry.isMap()) {
                final TypeMap propertyTypeMapping = typeMapping.getMapping(property);
                flow = (MapBuilder<?>) encode(entry.asMap(), flow.putMap(key),
                        propertyTypeMapping, mapping).end();
                continue;
            }

            final DataItem value = encode(entry, property, typeMapping, mapping);

            flow = flow.put(key, value);
        }

        object.parent();

        return flow;
    }

    final DataItem encode(final ValueCursor value, final String term, TypeMap typeMapping, Mapping mapping) throws EncoderError {

        if (value.isBoolean()) {
            return value.booleanValue() ? SimpleValue.TRUE : SimpleValue.FALSE;
        }

        if (value.isString()) {
            if (typeMapping != null) {
                final Collection<String> types = typeMapping.getType(term);

                for (final ValueEncoder valueEncoder : config.valueEncoders()) {
                    final DataItem dataItem = valueEncoder.encode(mapping, value, term, types);
                    if (dataItem != null) {
                        return dataItem;
                    }
                }
            }
            return new UnicodeString(value.stringValue());
        }

        if (value.isInteger()) {
            BigInteger integer = value.integerValue();

            switch (integer.signum()) {
            case -1:
                return new NegativeInteger(integer);
            case 0:
                return new UnsignedInteger(BigInteger.ZERO);
            case 1:
                return new UnsignedInteger(integer);
            }
        }

        if (value.isDecimal()) {
            return new DoublePrecisionFloat(value.decimalValue().doubleValue());
        }

        if (value.isNull()) {
            return SimpleValue.NULL;
        }

        throw new IllegalStateException();
    }

    final ArrayBuilder<?> encode(final ArrayCursor object, final ArrayBuilder<?> builder, String property, TypeMap typeMapping, Mapping mapping) throws EncoderError, JsonLdError {

        if (object.isEmpty()) {
            return builder;
        }

        ArrayBuilder<?> flow = builder;

        for (final ArrayItemCursor item : object) {

            if (item.isMap()) {
                flow = (ArrayBuilder<?>) encode(item.asMap(), flow.startMap(), typeMapping, mapping).end();
                continue;
            }

            if (item.isArray()) {
                flow = (ArrayBuilder<?>) encode(item.asArray(), flow.startArray(), property, typeMapping, mapping).end();
                continue;
            }

            final DataItem value = encode(item, property, typeMapping, mapping);

            flow = flow.add(value);
        }

        object.parent();

        return flow;
    }
}
