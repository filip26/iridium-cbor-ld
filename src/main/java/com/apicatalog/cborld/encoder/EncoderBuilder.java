package com.apicatalog.cborld.encoder;

import java.net.URI;
import java.util.Collection;

import com.apicatalog.cborld.config.Config;
import com.apicatalog.cborld.dictionary.CustomDictionary;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;

public class EncoderBuilder implements EncoderConfig {

    protected final EncoderConfig config;
    protected byte version;

    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected boolean compactArrays;
    protected URI base;

    public EncoderBuilder(EncoderConfig config) {
        this.config = config;
//        // default options
//        config(DefaultConfig.INSTANCE);
//
//        this.bundledContexts = DefaultConfig.STATIC_CONTEXTS;
//        this.version = DefaultConfig.VERSION;
//        this.base = null;
//        this.loader = null;
    }

    /**
     * If set to true, the encoder replaces arrays with just one element with that
     * element during encoding saving one byte. Enabled by default.
     *
     * @param enable <code>true</code> to enable arrays compaction
     * @return {@link EncoderBuilder} instance
     *
     */
    public EncoderBuilder compactArray(boolean enable) {
        this.compactArrays = enable;
        return this;
    }

    /**
     * Override any existing configuration by the given configuration set.
     * 
     * @param config a configuration set
     * @return {@link Encoder} instance
     */
    public EncoderBuilder config(Config config) {
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
     * @return {@link EncoderBuilder} instance
     */
    public EncoderBuilder loader(DocumentLoader loader) {
        this.loader = loader;
        return this;
    }

    /**
     * Use well-known contexts that are bundled with the library instead of fetching
     * it online. <code>true</code> by default. Disabling might cause slower
     * processing.
     *
     * @param enable <code>true</code> to use static bundled contexts
     * @return {@link EncoderBuilder} instance
     */
    public EncoderBuilder useBundledContexts(boolean enable) {
        this.bundledContexts = enable;
        return this;
    }

    /**
     * If set, then is used as the input document's base IRI.
     *
     * @param base a document base
     * @return {@link EncoderBuilder} instance
     */
    public EncoderBuilder base(URI base) {
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

    @Override
    public Collection<ValueEncoder> valueEncoders() {
//        return valueEncoders;
        return null;
    }

    @Override
    public EncoderMappingProvider encoderMapping() {
//        return provider;
        return null;
    }

    @Override
    public CustomDictionary dictionary() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public byte version() {
        // TODO Auto-generated method stub
        return 0;
    }

    public Encoder build() {
        if (loader == null) {
            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
            ((HttpLoader) loader).fallbackContentType(MediaType.JSON);
        }

        if (bundledContexts) {
            loader = new StaticContextLoader(loader);
        }
        return null;
    }

}
