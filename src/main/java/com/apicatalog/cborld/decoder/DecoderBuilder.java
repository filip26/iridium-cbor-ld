package com.apicatalog.cborld.decoder;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.config.LegacyConfigV05;
import com.apicatalog.cborld.config.LegacyConfigV06;
import com.apicatalog.cborld.encoder.EncoderBuilder;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;

public class DecoderBuilder {

    protected final CborLdVersion defaultVersion;
    protected final Map<CborLdVersion, DecoderConfig> decoders;

    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected URI base;

    protected DecoderBuilder(
            CborLdVersion defaultVersion,
            Map<CborLdVersion, DecoderConfig> decoders) {
        this.defaultVersion = defaultVersion;
        this.decoders = decoders;
        this.base = null;
        this.loader = null;
        this.bundledContexts = true;
    }

    public static final DecoderBuilder of(CborLdVersion... versions) {
        if (versions == null) {
            throw new IllegalArgumentException();
        }

        final Map<CborLdVersion, DecoderConfig> decoders = new HashMap<>();
        for (CborLdVersion version : versions) {
            enable(decoders, version);
        }

        return new DecoderBuilder(versions[0], decoders);
    }

    public static final DecoderBuilder of(DecoderConfig... configs) {
        if (configs == null) {
            throw new IllegalArgumentException();
        }

        final Map<CborLdVersion, DecoderConfig> decoders = new HashMap<>();
        for (DecoderConfig config : configs) {
            decoders.put(config.version(), config);
        }

        return new DecoderBuilder(configs[0].version(), decoders);
    }

    public DecoderBuilder enable(CborLdVersion version) {
        enable(decoders, version);
        return this;
    }

    public DecoderBuilder disable(CborLdVersion version) {
        decoders.remove(version);
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

//    @Override
//    public DocumentLoader loader() {
//        return loader;
//    }
//
//    @Override
//    public URI base() {
//        return base;
//    }

    /**
     * If set to true, the encoder replaces arrays with just one element with that
     * element during encoding saving one byte. Enabled by default.
     *
     * @param enable <code>true</code> to enable arrays compaction
     * @return {@link DecoderConfig} instance
     *
     */
    public DecoderBuilder compactArray(boolean enable) {
        return compactArray(defaultVersion, enable);
    }

    public DecoderBuilder compactArray(CborLdVersion version, boolean enable) {
//FIXME        this.compactArrays = enable;
        return this;
    }

    /**
     * Set a custom dictionary
     * 
     * @param dictionary
     * @return {@link EncoderBuilder} instance
     */
    public DecoderBuilder dictionary(DocumentDictionary dictionary) {
//        this.dictionary = dictionary;
        return this;
    }

    public Decoder build() {

        if (loader == null) {
            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
            ((HttpLoader) loader).fallbackContentType(MediaType.JSON);
        }

        if (bundledContexts) {
            loader = new StaticContextLoader(loader);
        }

        // only one?
        if (decoders.size() == 1) {
            return newInstance(decoders.values().iterator().next(), loader, base);
        }
        
        return new MultiDecoder(null); // FIXME
    }

    protected static final void enable(Map<CborLdVersion, DecoderConfig> decoders, CborLdVersion version) {
        switch (version) {
        case V1:
            decoders.put(version, DefaultConfig.INSTANCE);
            break;
        case V06:
            decoders.put(version, LegacyConfigV06.INSTANCE);
            break;
        case V05:
            decoders.put(version, LegacyConfigV05.INSTANCE);
            break;
        }
    }
    
    protected static final Decoder newInstance(DecoderConfig config, DocumentLoader loader, URI base) {
        switch (config.version()) {
        case V1:
            return new DecoderV1(config, loader, base);
        case V06:
            return new LegacyDecoderV06(config, loader, base);
        case V05:
            return new LegacyDecoderV05(config, loader, base);
        }
        throw new IllegalStateException();
    }
    
}
