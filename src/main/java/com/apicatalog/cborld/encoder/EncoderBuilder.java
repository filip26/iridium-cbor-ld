package com.apicatalog.cborld.encoder;

import java.net.URI;
import java.util.Collection;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.config.LegacyConfigV05;
import com.apicatalog.cborld.config.LegacyConfigV06;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;

public class EncoderBuilder implements EncoderConfig {

    protected EncoderMappingProvider provider;

    protected DocumentDictionary dictionary;

    protected Collection<ValueEncoder> valueEncoders;

    protected CborLdVersion version;

    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected boolean compactArrays;
    protected URI base;

    protected EncoderBuilder(EncoderConfig config) {
        this.provider = config.encoderMapping();
        this.compactArrays = config.isCompactArrays();
        this.valueEncoders = config.valueEncoders();
        this.dictionary = config.dictionary();
        this.version = config.version();
        this.base = null;
        this.loader = null;
        this.bundledContexts = true;
    }
    
    public static EncoderBuilder of(EncoderConfig config) {
        return new EncoderBuilder(config);
    }
    
    public static EncoderBuilder of(CborLdVersion version) {
        return new EncoderBuilder(config(version));
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
     * @param version
     * @return {@link EncoderBuilder} instance
     */
    public EncoderBuilder version(CborLdVersion version) {
        this.version = version;
        return this;
    }

    @Override
    public boolean isCompactArrays() {
        return compactArrays;
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
    public CborLdVersion version() {
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
        if (version == CborLdVersion.V05 || version == CborLdVersion.V06) {
            return new LegacyEncoder(this, loader, base);            
        }
        return new LegacyEncoder(this, loader, base);
    }
    
    protected static final EncoderConfig config(CborLdVersion version) {
        switch (version) {
        case V1:
            return DefaultConfig.INSTANCE;
        case V06:
            return LegacyConfigV06.INSTANCE;
        case V05:
            return LegacyConfigV05.INSTANCE;
        }
        return DefaultConfig.INSTANCE;
    }

}
