package com.apicatalog.cborld.encoder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.net.URI;
import java.util.Collection;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.config.DictionaryAlgorithm;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.encoder.EncoderError.Code;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.cborld.mapper.Mapping;
import com.apicatalog.cborld.mapper.MappingProvider;
import com.apicatalog.cborld.mapper.TypeMap;
import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.ArrayItemCursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.MapEntryCursor;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
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

public class Encoder implements EncoderConfig {

    protected final MapCursor document;

    protected MappingProvider provider;
    protected Dictionary index;

    // options
    protected Collection<ValueEncoder> valueEncoders;
    protected boolean compactArrays;
    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected URI base;
    
    protected Encoder(MapCursor document) {
        this.document = document;
    
        // default options
        config(DefaultConfig.INSTANCE);
        
        this.bundledContexts = DefaultConfig.STATIC_CONTEXTS;
        this.base = null;
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

    /**
     * Override any existing configuration by the given configuration set.
     * 
     * @param config a configuration set 
     * @return {@link Encoder} instance
     */
    public Encoder config(EncoderConfig config) {
        this.compactArrays = config.isCompactArrays();
        this.valueEncoders = config.valueEncoders();
        this.provider = config.provider();
        return this;
    }
    
    /**
     * Set {@link DocumentLoader} used to fetch referenced JSON-LD contexts. 
     * If not set then default document loader provided by {@link JsonLdOptions} is used. 
     * 
     * @param loader a document loader to set
     * @return {@link Encoder} instance
     */
    public Encoder loader(DocumentLoader loader) {
        this.loader = loader;
        return this;
    }
    

    /**
     * Use well-known contexts that are bundled with the library instead of fetching it online.
     * <code>true</code> by default. Disabling might cause slower processing.
     *
     * @param enable
     * @return {@link Encoder} instance
     */
    public Encoder useBundledContexts(boolean enable) {
        this.bundledContexts = enable;
        return this;
    }
    
    /**
     * If set, then is used as the input document's base IRI.
     *
     * @param base
     * @return {@link Encoder} instance
     */
    public Encoder base(URI base) {
       this.base = base;
       return this;
    }

    /**
     * Encode JSON-LD document as CBOR-LD document.
     * 
     * @return a byte array representing the encoded CBOR-LD document.
     * 
     * @throws EncoderError
     * @throws ContextError
     */
    public byte[] encode() throws EncoderError, ContextError {
    
        if (loader == null) {
            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
            ((HttpLoader)loader).setFallbackContentType(MediaType.JSON);
        }
        
        if (bundledContexts) {
            loader = new StaticContextLoader(loader);
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
     * Compress the given JSON-LD document into CBOR-LD byte array.
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
            final Mapping mapping = provider.getEncoderMapping(document, base, loader, this);

            index = mapping.dictionary();
            
            final CborBuilder builder = (CborBuilder) encode(document, new CborBuilder().addMap(), mapping.typeMap()).end();
            
            // 2.CBOR Tag - 0xD9, CBOR-LD - 0x05, Compressed - CBOR-LD compression algorithm
            // version 1 - 0x01
            baos.write(CborLd.CBOR_LD_BYTE_PREFIX);
            baos.write(CborLd.COMPRESSED);
            
            new CborEncoder(baos).encode(builder.build());

            return baos.toByteArray();

        } catch (CborException e) {
            throw new EncoderError(Code.InvalidDocument, e);

        } catch (IOException | JsonLdError e) {
            throw new EncoderError(Code.Internal, e);
        }
    }

    final MapBuilder<?> encode(final MapCursor object, final MapBuilder<?> builder, TypeMap typeMapping) throws EncoderError, JsonLdError {

        MapBuilder<?> flow = builder;
    
        for (final MapEntryCursor entry  : object) {
            
            final String property = entry.mapKey();
    
            final BigInteger encodedProperty = index.getCode(property);
                
            if (entry.isArray()) {
                    
                final DataItem key = encodedProperty != null
                            ? new UnsignedInteger(encodedProperty.add(BigInteger.ONE))
                            : new UnicodeString(property);
                
                if (compactArrays && object.asArray().size() == 1) {
        
                    object.asArray().item(0);
        
                    if (object.isMap()) {
                        final TypeMap propertyTypeMapping = typeMapping.getMapping(property);
                        flow = (MapBuilder<?>) encode(object.asMap(), flow.putMap(key), propertyTypeMapping).end();
        
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
                continue;
            }
    
            final DataItem key = encodedProperty != null
                ? new UnsignedInteger(encodedProperty)
                : new UnicodeString(property);
    
            if (entry.isMap()) {
                final TypeMap propertyTypeMapping = typeMapping.getMapping(property);
                flow = (MapBuilder<?>) encode(entry.asMap(), flow.putMap(key),
                    propertyTypeMapping
                    ).end();
                continue;
            }
    
            final DataItem value = encode(entry, property, typeMapping);
    
            flow = flow.put(key, value);
        }

        object.parent();
        
        return flow;
    }

    final DataItem encode(final ValueCursor value, final String term, TypeMap typeMapping) throws EncoderError {
    
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

    final ArrayBuilder<?> encode(final ArrayCursor object, final ArrayBuilder<?> builder, String property, TypeMap typeMapping) throws EncoderError, JsonLdError {
    
        ArrayBuilder<?> flow = builder;
    
        for (final ArrayItemCursor item : object) {
    
            if (item.isMap()) {
                flow = (ArrayBuilder<?>) encode(item.asMap(), flow.startMap(), typeMapping).end();
                continue;
            }
    
            if (item.isArray()) {
                flow = (ArrayBuilder<?>) encode(item.asArray(), flow.startArray(), property, typeMapping).end();
                continue;
            }

            final DataItem value = encode(item, property, typeMapping);
    
            flow = flow.add(value);
        }
        
        object.parent();
        
        return flow;
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
    public Collection<ValueEncoder> valueEncoders() {
        return valueEncoders;
    }

    @Override
    public MappingProvider provider() {
        return provider;
    }
}
