package com.apicatalog.cborld.decoder;

import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.dictionary.DocumentDictionary;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;

public class DecoderBuilder implements DecoderConfig {

    protected DecoderMappingProvider provider;

    protected Map<Integer, DocumentDictionary> dictionaries;

    protected Collection<ValueDecoder> valueDecoders;

    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected boolean compactArrays;
    protected URI base;

    public DecoderBuilder(DecoderConfig config) {
        this.provider = config.decoderMapping();
        this.compactArrays = config.isCompactArrays();
        this.valueDecoders = config.valueDecoders();
        this.dictionaries = config.dictionaries() != null
                ? new HashMap<>(config.dictionaries())
                : new HashMap<>();
        this.base = config.base();
        this.loader = config.loader();
        this.bundledContexts = DefaultConfig.STATIC_CONTEXTS;
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
    public DecoderBuilder dictionary(DocumentDictionary dictionary) {
        return dictionary(dictionary.code(), dictionary);
    }

    /**
     * Add new terms dictionary
     * 
     * @param code       CBOR-LD terms dictionary code
     * @param dictionary a custom dictionary
     * @return {@link DecoderBuilder} instance
     */
    public DecoderBuilder dictionary(int code, DocumentDictionary dictionary) {
        if (dictionaries == null) {
            dictionaries = new HashMap<>();
        }
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
    public Map<Integer, DocumentDictionary> dictionaries() {
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
