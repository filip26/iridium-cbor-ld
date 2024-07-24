package com.apicatalog.cborld.config;

import java.net.URI;

import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.loader.DocumentLoader;

public class BaseConfig implements Config {

    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected boolean compactArrays;
    protected URI base;

    protected BaseConfig() {
        this.bundledContexts = DefaultConfig.STATIC_CONTEXTS;
        this.compactArrays = DefaultConfig.COMPACT_ARRAYS;
        this.loader = null;
        this.base = null;
    }
    
    /**
     * If set to true, the encoder replaces arrays with just one element with that
     * element during encoding saving one byte. Enabled by default.
     *
     * @param enable <code>true</code> to enable arrays compaction
     * @return {@link BaseConfig} instance
     *
     */
    public BaseConfig compactArray(boolean enable) {
        compactArrays = enable;
        return this;
    }

    /**
     * Override any existing configuration by the given configuration set.
     * 
     * @param config a configuration set
     * @return {@link Encoder} instance
     */
    public BaseConfig config(Config config) {
        this.compactArrays = config.isCompactArrays();
        this.base = config.base();
        this.loader = config.loader();
        return this;
    }

    /**
     * Set {@link DocumentLoader} used to fetch referenced JSON-LD contexts. If not
     * set then default document loader provided by {@link JsonLdOptions} is used.
     * 
     * @param loader a document loader to set
     * @return {@link BaseConfig} instance
     */
    public BaseConfig loader(DocumentLoader loader) {
        this.loader = loader;
        return this;
    }

    /**
     * Use well-known contexts that are bundled with the library instead of fetching
     * it online. <code>true</code> by default. Disabling might cause slower
     * processing.
     *
     * @param enable <code>true</code> to use static bundled contexts
     * @return {@link BaseConfig} instance
     */
    public BaseConfig useBundledContexts(boolean enable) {
        this.bundledContexts = enable;
        return this;
    }

    /**
     * If set, then is used as the input document's base IRI.
     *
     * @param base a document base
     * @return {@link BaseConfig} instance
     */
    public BaseConfig base(URI base) {
        this.base = base;
        return this;
    }

    @Override
    public boolean isCompactArrays() {
        return compactArrays;
    }

    @Override
    public DocumentLoader loader() {
        return loader;
    }

    @Override
    public URI base() {
        return base;
    }
}
