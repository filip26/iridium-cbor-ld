package com.apicatalog.cborld.encoder;

import java.net.URI;
import java.util.Collection;

import com.apicatalog.cborld.CborLd.Version;
import com.apicatalog.cborld.config.ConfigV1;
import com.apicatalog.cborld.config.LegacyConfigV05;
import com.apicatalog.cborld.config.LegacyConfigV06;
import com.apicatalog.cborld.debug.DebugEncoder;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.loader.DocumentLoader;

/**
 * Builder for creating and configuring {@link Encoder} instances.
 *
 * <p>
 * The {@code EncoderBuilder} allows customization of CBOR-LD encoding behavior,
 * including context loading, dictionary usage, value encoding, and compression
 * options. Builders are typically created via:
 * </p>
 *
 * <pre>{@code
 * Encoder encoder = CborLd.createEncoder()
 *         .dictionary(dictionary)
 *         .compactArray(true)
 *         .build();
 * }</pre>
 *
 * <p>
 * This class also implements {@link EncoderConfig}, exposing the internal
 * configuration state to encoder instances.
 * </p>
 */
public final class EncoderBuilder implements EncoderConfig {

    private EncoderMappingProvider provider;

    private DocumentDictionary dictionary;

    private Collection<ValueEncoder> valueEncoders;

    private Version version;

    private DocumentLoader loader;
    private boolean bundledContexts;
    private boolean compactArrays;
    private URI base;

    /**
     * Constructs a new {@code EncoderBuilder} initialized with the given
     * configuration.
     *
     * @param config the base encoder configuration to initialize from
     */
    EncoderBuilder(EncoderConfig config) {
        this.provider = config.encoderMapping();
        this.compactArrays = config.isCompactArrays();
        this.valueEncoders = config.valueEncoders();
        this.dictionary = config.dictionary();
        this.version = config.version();
        this.base = null;
        this.loader = null;
        this.bundledContexts = true;
    }

    /**
     * Enables or disables array compaction. When enabled, arrays with a single
     * element are encoded as scalars.
     *
     * @param enable {@code true} to enable compaction, {@code false} otherwise
     * @return this builder instance
     */
    public EncoderBuilder compactArray(boolean enable) {
        this.compactArrays = enable;
        return this;
    }

    /**
     * Set {@link DocumentLoader} used to fetch referenced JSON-LD contexts. If not
     * set then default document loader provided by {@link Options} is used.
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
     * Sets the base URI used for resolving relative URI.
     *
     * @param base the base URI
     * @return this builder instance
     */
    public EncoderBuilder base(URI base) {
        this.base = base;
        return this;
    }

    /**
     * Sets a static dictionary used for compression.
     *
     * @param dictionary the dictionary to use
     * @return this builder instance
     */
    public EncoderBuilder dictionary(DocumentDictionary dictionary) {
        this.dictionary = dictionary;
        return this;
    }

    /**
     * Sets the CBOR-LD version to use for encoding.
     *
     * @param version the version to use
     * @return this builder instance
     */
    public EncoderBuilder version(Version version) {
        this.version = version;
        return this;
    }

    /**
     * Builds and returns a new {@link Encoder} instance using the current
     * configuration.
     *
     * @return a fully configured {@link Encoder}
     */
    public Encoder build() {
        if (loader == null) {
//            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
//            ((HttpLoader) loader).fallbackContentType(MediaType.JSON);
        }

        if (bundledContexts) {
//            loader = new StaticContextLoader(loader);
        }
        return new DefaultEncoder(this, provider, loader, base);
    }

    /**
     * Builds and returns a {@link DebugEncoder} instance for diagnostics and
     * inspection.
     *
     * @return a debug encoder instance
     */
    public DebugEncoder debug() {
        if (loader == null) {
//            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
//            ((HttpLoader) loader).fallbackContentType(MediaType.JSON);
        }

        if (bundledContexts) {
//            loader = new StaticContextLoader(loader);
        }
        return new DebugEncoder(this, loader, base);
    }

    /**
     * Returns the default encoder configuration for the specified version.
     *
     * @param version the CBOR-LD version
     * @return the encoder configuration
     */
    static final EncoderConfig config(Version version) {
        switch (version) {
        case V06:
            return LegacyConfigV06.INSTANCE;
        case V05:
            return LegacyConfigV05.INSTANCE;
        case V1:
        default:
            break;
        }
        return ConfigV1.INSTANCE;
    }

    // --- EncoderConfig interface methods ---

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
    public Version version() {
        return version;
    }
}
