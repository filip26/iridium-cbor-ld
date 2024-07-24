package com.apicatalog.cborld.decoder;

import java.net.URI;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.dictionary.ContextDictionary;
import com.apicatalog.cborld.dictionary.CustomDictionary;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;

public class DecoderBuilder implements DecoderConfig {

    protected DecoderMappingProvider provider;

    protected Map<Integer, CustomDictionary> dictionaries;

    protected Collection<ValueDecoder> valueDecoders;

    protected Dictionary contexts;
    protected Map<String, Dictionary> types;

    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected boolean compactArrays;
    protected URI base;

    public DecoderBuilder(DecoderConfig config) {
        this.provider = config.decoderMapping();
        this.compactArrays = config.isCompactArrays();
        this.valueDecoders = config.valueDecoders();

        this.dictionaries = new LinkedHashMap<>();
        this.dictionaries.put(0x01, new CustomDictionary(0x01, ContextDictionary.INSTANCE, null));
        this.bundledContexts = DefaultConfig.STATIC_CONTEXTS;
        this.base = null;
        this.loader = null;
    }

    /**
     * If set to true, the encoder replaces arrays with just one element with that
     * element during encoding saving one byte. Enabled by default.
     *
     * @param enable <code>true</code> to enable arrays compaction
     * @return {@link DecoderBuilder} instance
     *
     */
    public DecoderBuilder compactArray(boolean enable) {
        compactArrays = enable;
        return this;
    }

    /**
     * Override any existing configuration by the given configuration set.
     * 
     * @param config a configuration set
     * @return {@link Encoder} instance
     */
    public DecoderBuilder config(DecoderConfig config) {
        this.provider = config.decoderMapping();
        this.compactArrays = config.isCompactArrays();
        this.valueDecoders = config.valueDecoders();
        this.dictionaries = config.dictionaries();
        return this;
    }

    /**
     * Set {@link DocumentLoader} used to fetch referenced JSON-LD contexts. If not
     * set then default document loader provided by {@link JsonLdOptions} is used.
     * 
     * @param loader a document loader to set
     * @return {@link DecoderBuilder} instance
     */
    public DecoderBuilder loader(DocumentLoader loader) {
        this.loader = loader;
        return this;
    }

    /**
     * Use well-known contexts that are bundled with the library instead of fetching
     * it online. <code>true</code> by default. Disabling might cause slower
     * processing.
     *
     * @param enable <code>true</code> to use static bundled contexts
     * @return {@link DecoderBuilder} instance
     */
    public DecoderBuilder useBundledContexts(boolean enable) {
        this.bundledContexts = enable;
        return this;
    }

    /**
     * If set, then is used as the input document's base IRI.
     *
     * @param base a document base
     * @return {@link DecoderBuilder} instance
     */
    public DecoderBuilder base(URI base) {
        this.base = base;
        return this;
    }

    /**
     * Add new terms dictionary
     * 
     * @param dictionary a custom dictionary
     * @return {@link DecoderBuilder} instance
     */
    public DecoderBuilder dictionary(CustomDictionary dictionary) {
        return dictionary(dictionary.code(), dictionary);
    }

    /**
     * Add new terms dictionary
     * 
     * @param code       CBOR-LD terms dictionary code
     * @param dictionary a custom dictionary
     * @return {@link DecoderBuilder} instance
     */
    public DecoderBuilder dictionary(int code, CustomDictionary dictionary) {
        dictionaries.put(code, dictionary);
        return this;
    }

    @Override
    public boolean isCompactArrays() {
        return compactArrays;
    }

    @Override
    public Collection<ValueDecoder> valueDecoders() {
        return valueDecoders;
    }

    @Override
    public DecoderMappingProvider decoderMapping() {
        return provider;
    }

    @Override
    public Map<Integer, CustomDictionary> dictionaries() {
        return dictionaries;
    }
    
    @Override
    public DocumentLoader loader() {
        return loader;
    }

    @Override
    public URI base() {
        return base;
    }
    
    public Decoder build() {
        
        if (loader == null) {
            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
            ((HttpLoader) loader).fallbackContentType(MediaType.JSON);
        }
        
        if (bundledContexts) {
            loader = new StaticContextLoader(loader); 
        }

        return new Decoder(this);
    }
}
