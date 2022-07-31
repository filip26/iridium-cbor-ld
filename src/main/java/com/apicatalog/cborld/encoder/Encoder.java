package com.apicatalog.cborld.encoder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Collection;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.config.DefaultEncoderConfig;
import com.apicatalog.cborld.context.Context;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.context.TypeMapping;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.encoder.EncoderError.Code;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.jsonld.JsonLdError;
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

    protected final MapCursor document;

    protected CodeTermMap index;

    // options
    protected Collection<ValueEncoder> valueEncoders;
    protected boolean compactArrays;
    protected DocumentLoader loader;
    
    protected Encoder(MapCursor document) {
        this.document = document;
    
        // default options
        this.valueEncoders = DefaultEncoderConfig.VALUE_ENCODERS;
        this.compactArrays = DefaultEncoderConfig.COMPACT_ARRAYS;
        this.loader = null;
    }

    public static final Encoder create(MapCursor document) throws EncoderError {
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
        
        loader = new StaticContextLoader(loader);
        
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
    final byte[] compress(final MapCursor document, Collection<String> contextUrls) throws ContextError, EncoderError {
    
        // 1.
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    
        try {
            // 2.CBOR Tag - 0xD9, CBOR-LD - 0x05, Compressed - CBOR-LD compression algorithm
            // version 1 - 0x01
            baos.write(CborLd.CBOR_LD_BYTE_PREFIX);
            baos.write(CborLd.COMPRESSED);
                    
            final Context context = Context.from(document, loader);
            
            index = CodeTermMap.from(contextUrls, context.getContextKeySets(), loader);
            
            final CborBuilder builder = (CborBuilder) encode(document, new CborBuilder().addMap(), context.getTypeMapping()).end();
            
            new CborEncoder(baos).encode(builder.build());

            return baos.toByteArray();

        } catch (CborException e) {
            throw new EncoderError(Code.InvalidDocument, e);

        } catch (IOException | JsonLdError e) {
            throw new EncoderError(Code.Internal, e);
        }
    }

    final MapBuilder<?> encode(final MapCursor object, final MapBuilder<?> builder, TypeMapping typeMapping) throws EncoderError, JsonLdError {

        MapBuilder<?> flow = builder;
    
        for (final String property : object.keys()) {
    
            final BigInteger encodedProperty = index.getCode(property);
                
            if (object.isArray(property)) {
                    
                final DataItem key = encodedProperty != null
                            ? new UnsignedInteger(encodedProperty.add(BigInteger.ONE))
                            : new UnicodeString(property);
        
                object.array(property);
        
                if (compactArrays && object.asArray().size() == 1) {
        
                    object.asArray().value(0);
        
                    if (object.isObject()) {
                        final TypeMapping propertyTypeMapping = typeMapping.getMapping(property);
                        flow = (MapBuilder<?>) encode(object.asObject(), flow.putMap(key), propertyTypeMapping).end();
        
                    } else if (object.isArray()) {
                        flow = (MapBuilder<?>) encode(
                                                    object.asArray(), 
                                                    flow.putArray(key),
                                                    property,
                                                    typeMapping
                                                    ).end();
        
                    } else {
                        final DataItem value = encode(object, property, typeMapping);
        
                        flow = flow.put(key, value);
                    }
        
                    object.parent();
        
                } else {
                    flow = (MapBuilder<?>) encode(object.asArray(), flow.putArray(key),
                        property,
                        typeMapping
                        ).end();
                }
        
                object.parent();
                continue;
            }
    
            final DataItem key = encodedProperty != null
                ? new UnsignedInteger(encodedProperty)
                : new UnicodeString(property);
    
            if (object.isObject(property)) {
                final TypeMapping propertyTypeMapping = typeMapping.getMapping(property);
                flow = (MapBuilder<?>) encode(object.object(property), flow.putMap(key),
                    propertyTypeMapping
                    ).end();
        
                object.parent();
                continue;
            }
    
            final DataItem value = encode(object.value(property), property, typeMapping);
    
            flow = flow.put(key, value);
    
            object.parent();
        }
        return flow;
    }

    final DataItem encode(final ValueCursor value, final String term, TypeMapping typeMapping) throws EncoderError {
    
        if (value.isBoolean()) {
            return value.booleanValue() ? SimpleValue.TRUE : SimpleValue.FALSE;
        }

        if (value.isString()) {
            if (typeMapping != null) { 
                final Collection<String> types = typeMapping.getType(term);
                
                for (final ValueEncoder valueEncoder : valueEncoders) {
                    final DataItem dataItem = valueEncoder.encode(index, value, term, types);
                    if (dataItem != null) {
                        return dataItem;
                    }
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

    final ArrayBuilder<?> encode(final ArrayCursor object, final ArrayBuilder<?> builder, String property, TypeMapping typeMapping) throws EncoderError, JsonLdError {
    
        ArrayBuilder<?> flow = builder;
    
        for (int i = 0; i < object.size(); i++) {
    
            if (object.isObject(i)) {
                flow = (ArrayBuilder<?>) encode(object.object(i), flow.startMap(), typeMapping).end();
                object.parent();
                continue;
            }
    
            if (object.isArray(i)) {
                flow = (ArrayBuilder<?>) encode(object.array(i), flow.startArray(), property, typeMapping).end();
                object.parent();
                continue;
            }
    
            DataItem value = encode(object.value(i), property, typeMapping);
            object.parent();
    
            flow = flow.add(value);
        }
        return flow;
    }
}
