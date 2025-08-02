package com.apicatalog.cborld.decoder;

import java.net.URI;

import com.apicatalog.cborld.config.Config;
import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;

public class DecoderBuilder implements Config {


    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected URI base;

    public DecoderBuilder(Config config) {
        this.base = config.base();
        this.loader = config.loader();
        this.bundledContexts = DefaultConfig.STATIC_CONTEXTS;
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

    @Override
    public DocumentLoader loader() {
        return loader;
    }

    @Override
    public URI base() {
        return base;
    }
    
    /**
     * If set to true, the encoder replaces arrays with just one element with that
     * element during encoding saving one byte. Enabled by default.
     *
     * @param enable <code>true</code> to enable arrays compaction
     * @return {@link DecoderConfig} instance
     *
     */
    public DecoderBuilder compactArray(boolean enable) {
//FIXME        this.compactArrays = enable;
        return this;
    }

    public MultiDecoder build() {

        if (loader == null) {
            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
            ((HttpLoader) loader).fallbackContentType(MediaType.JSON);
        }

        if (bundledContexts) {
            loader = new StaticContextLoader(loader);
        }

        return new MultiDecoder(null);  //FIXME
    }
}
