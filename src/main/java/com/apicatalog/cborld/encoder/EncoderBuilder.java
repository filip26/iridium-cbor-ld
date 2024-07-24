package com.apicatalog.cborld.encoder;

import java.net.URI;
import java.util.Collection;

import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.document.DocumentDictionary;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;

public class EncoderBuilder implements EncoderConfig {

    protected EncoderMappingProvider provider;

    protected DocumentDictionary dictionary;

    protected Collection<ValueEncoder> valueEncoders;

    protected byte version;

    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected boolean compactArrays;
    protected URI base;

    public EncoderBuilder(EncoderConfig config) {
        this.provider = config.encoderMapping();
        this.compactArrays = config.isCompactArrays();
        this.valueEncoders = config.valueEncoders();
        this.dictionary = config.dictionary();
        this.version = config.version();
        this.base = config.base();
        this.loader = config.loader();
        this.bundledContexts = DefaultConfig.STATIC_CONTEXTS;
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

    /**
     * Set a custom dictionary
     * 
     * @param dictionary
     * @return {@link EncoderBuilder} instance
     */
    public EncoderBuilder dictionary(DocumentDictionary dictionary) {
        this.dictionary = dictionary;
        return this;
    }

    /**
     * Set encoding version
     * 
     * @param dictionary
     * @return {@link EncoderBuilder} instance
     */
    public EncoderBuilder version(byte version) {
        this.version = version;
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
        return valueEncoders;
    }

    @Override
    public EncoderMappingProvider encoderMapping() {
        return provider;
    }

    @Override
    public DocumentDictionary dictionary() {
        return dictionary;
    }

    @Override
    public byte version() {
        return version;
    }

    public Encoder build() {
        if (loader == null) {
            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
            ((HttpLoader) loader).fallbackContentType(MediaType.JSON);
        }

        if (bundledContexts) {
            loader = new StaticContextLoader(loader);
        }
        return new Encoder(this);
    }
}
