package com.apicatalog.cborld.encoder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Collection;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.encoder.EncoderError.Code;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.json.cursor.JsonArrayCursor;
import com.apicatalog.json.cursor.JsonObjectCursor;
import com.apicatalog.json.cursor.JsonValueCursor;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;

import co.nstant.in.cbor.CborBuilder;
import co.nstant.in.cbor.CborEncoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.builder.ArrayBuilder;
import co.nstant.in.cbor.builder.MapBuilder;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.SimpleValue;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

public class Encoder {

    protected final JsonObjectCursor document;

    protected CodeTermMap index;

    // options
    protected Collection<ValueEncoder> valueEncoders;
    protected boolean compactArrays;
    protected DocumentLoader loader;

    protected Encoder(JsonObjectCursor document) {
        this.document = document;
    
        // default options
        this.valueEncoders = DefaultEncoderConfig.VALUE_ENCODERS;
        this.compactArrays = DefaultEncoderConfig.COMPACT_ARRAYS;
        this.loader = null;
    }

    public static final Encoder create(JsonObjectCursor document) throws EncoderError {
        if (document == null) {
            throw new IllegalArgumentException("The 'document' parameter must not be null.");
        }
    
        return new Encoder(document);
    }

    /**
     * If set to true, the encoder replaces arrays with
     * just one element with that element during encoding saving one byte.
     * Enabled by default.
     *
     * @param enable <code>true</code> to enable arrays compaction
     * @return {@link Encoder} instance
     *
     */
    public Encoder compactArray(boolean enable) {
        compactArrays = enable;
        return this;
    }

    public Encoder config(EncoderConfigration config) {
        compactArrays = config.isCompactArrays();
        valueEncoders = config.getValueEncoders();
        return this;
    }
    
    public Encoder loader(DocumentLoader loader) {
        this.loader = loader;
        return this;
    }

    public byte[] encode() throws EncoderError, ContextError {
    
        if (loader == null) {
            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
            ((HttpLoader)loader).setFallbackContentType(MediaType.JSON);
        }
        
        try {
    
            final Collection<String> contexts = EncoderContext.get(document);
    
            if (contexts.isEmpty()) { // is not JSON-LD document
            throw new EncoderError(Code.InvalidDocument, "Not a valid JSON-LD document in a compacted form.");
            }
    
            return compress(document, contexts);
    
        // non compressable context
        } catch (IllegalArgumentException e) {
            System.out.println("TODO: non-compressable");
        }
    
        return null;
    }

    /**
     * Compresses the given JSON-LD document into CBOR-LD byte array.
     *
     * @see <a href=
     *      "https://digitalbazaar.github.io/cbor-ld-spec/#compressed-cbor-ld-buffer-algorithm">Compressed
     *      CBOR-LD Buffer Algorithm</a>
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
    final byte[] compress(final JsonObjectCursor document, Collection<String> contextUrls) throws ContextError, EncoderError {
    
        // 1.
        final ByteArrayOutputStream result = new ByteArrayOutputStream();
    
        try {
            // 2.CBOR Tag - 0xD9, CBOR-LD - 0x05, Compressed - CBOR-LD compression algorithm
            // version 1 - 0x01
            result.write(CborLd.CBOR_LD_BYTE_PREFIX);
            result.write(CborLd.COMPRESSED);
    
            index = CodeTermMap.from(contextUrls, loader);
    
            return toCbor(document, result);
    
        } catch (IOException e) {
            throw new EncoderError(Code.Internal, e);
        }
    }

    final byte[] toCbor(JsonObjectCursor object, ByteArrayOutputStream baos) throws EncoderError {
    
        try {
            final CborBuilder builder = (CborBuilder) toCbor(object, new CborBuilder().addMap(), null).end();
    
            new CborEncoder(baos).encode(builder.build());
    
        } catch (CborException e) {
            throw new EncoderError(Code.InvalidDocument, e);
        }
    
        return baos.toByteArray();
    }

    final MapBuilder<?> toCbor(final JsonObjectCursor object, final MapBuilder<?> builder, TermDefinition def) throws EncoderError {
    
        MapBuilder<?> flow = builder;
    
        for (final String property : object.properies()) {
    
            final BigInteger encodedProperty = index.getCode(property);
    
            if (object.isArray(property)) {
    
                final DataItem key = encodedProperty != null
                            ? new UnsignedInteger(encodedProperty.add(BigInteger.ONE))
                            : new UnicodeString(property);
        
                object.array(property);
        
                if (compactArrays && object.asArray().size() == 1) {
        
                    object.asArray().value(0);
        
                    if (object.isObject()) {
                        flow = (MapBuilder<?>) toCbor(object.asObject(), flow.putMap(key), index.getDefinition(def, property)).end();
        
                    } else if (object.isArray()) {
                        flow = (MapBuilder<?>) toCbor(
                                                    object.asArray(), 
                                                    flow.putArray(key),
                                                    property,
                                                    index.getDefinition(def, property)).end();
        
                    } else {
                        final DataItem value = toCbor(object, property, index.getDefinition(def, property));
        
                        flow = flow.put(key, value);
                    }
        
                    object.parent();
        
                } else {
                    flow = (MapBuilder<?>) toCbor(object.asArray(), flow.putArray(key),
                        property,
                        index.getDefinition(def, property)
                        ).end();
                }
        
                object.parent();
                continue;
            }
    
            final DataItem key = encodedProperty != null
                ? new UnsignedInteger(encodedProperty)
                : new UnicodeString(property);
    
            if (object.isObject(property)) {
    
                flow = (MapBuilder<?>) toCbor(object.object(property), flow.putMap(key),
                    index.getDefinition(def, property)
                    ).end();
        
                object.parent();
                continue;
            }
    
            final DataItem value = toCbor(object.value(property), property, index.getDefinition(def, property));
    
            flow = flow.put(key, value);
    
            object.parent();
        }
        return flow;
    }

    final DataItem toCbor(final JsonValueCursor value, final String term, final TermDefinition def) throws EncoderError {
    
        if (value.isBoolean()) {
            return value.booleanValue() ? SimpleValue.TRUE : SimpleValue.FALSE;
        }
    
        if (value.isString()) {
    
            //TODO better
            for (final ValueEncoder valueEncoder : valueEncoders) {
            DataItem dataItem = valueEncoder.encode(index, value, term, def);
            if (dataItem != null) {
                return dataItem;
            }
            }
            return new UnicodeString(value.stringValue());
        }
    
        if (value.isNumber()) {
            //TODO
            return new UnsignedInteger(value.integerValue());
        }
    
        throw new IllegalStateException("TODO " + value);
    }

    final ArrayBuilder<?> toCbor(final JsonArrayCursor object, final ArrayBuilder<?> builder, String property, TermDefinition def) throws EncoderError {
    
        ArrayBuilder<?> flow = builder;
    
        for (int i = 0; i < object.size(); i++) {
    
            if (object.isObject(i)) {
                flow = (ArrayBuilder<?>) toCbor(object.object(i), flow.startMap(), def).end();
                object.parent();
                continue;
            }
    
            if (object.isArray(i)) {
                flow = (ArrayBuilder<?>) toCbor(object.array(i), flow.startArray(), property, def).end();
                object.parent();
                continue;
            }
    
            DataItem value = toCbor(object.value(i), property, def);
            object.parent();
    
            flow = flow.add(value);
        }
        return flow;
    }
}
