package com.apicatalog.cborld.decoder;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.config.ConfigV1;
import com.apicatalog.cborld.config.LegacyConfigV05;
import com.apicatalog.cborld.config.LegacyConfigV06;
import com.apicatalog.cborld.debug.DebugDecoder;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;

/**
 * Builder class for creating and configuring {@link Decoder} instances.
 * <p>
 * Supports configuration of multiple CBOR-LD versions, base URI, dictionary,
 * array compaction behavior, context loading strategy, and document loaders.
 */
public class DecoderBuilder {

    protected final CborLdVersion defaultVersion;
    protected final Map<CborLdVersion, DecoderConfigBuilder> versions;

    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected URI base;

    protected DecoderBuilder(
            CborLdVersion defaultVersion,
            Map<CborLdVersion, DecoderConfigBuilder> decoders) {
        this.defaultVersion = defaultVersion;
        this.versions = decoders;
        this.base = null;
        this.loader = null;
        this.bundledContexts = true;
    }

    /**
     * Creates a new {@code DecoderBuilder} instance with the given CBOR-LD format
     * versions enabled.
     *
     * @param versions one or more CBOR-LD versions to support
     * @return a new {@code DecoderBuilder} instance
     * @throws IllegalArgumentException if {@code versions} is {@code null} or empty
     */
    public static final DecoderBuilder of(CborLdVersion... versions) {
        if (versions == null || versions.length == 0) {
            throw new IllegalArgumentException();
        }

        final Map<CborLdVersion, DecoderConfigBuilder> decoders = new HashMap<>();
        for (CborLdVersion version : versions) {
            enable(decoders, version);
        }

        return new DecoderBuilder(versions[0], decoders);
    }

    /**
     * Creates a new {@code DecoderBuilder} instance pre-configured with the given
     * decoder configurations.
     *
     * @param configs one or more decoder configurations
     * @return a new {@code DecoderBuilder} instance
     * @throws IllegalArgumentException if {@code configs} is {@code null} or empty
     */
    public static final DecoderBuilder of(DecoderConfig... configs) {
        if (configs == null || configs.length == 0) {
            throw new IllegalArgumentException();
        }

        final Map<CborLdVersion, DecoderConfigBuilder> decoders = new HashMap<>();
        for (DecoderConfig config : configs) {
            decoders.put(config.version(), DecoderConfigBuilder.of(config));
        }

        return new DecoderBuilder(configs[0].version(), decoders);
    }

    /**
     * Enables support for the specified CBOR-LD version.
     *
     * @param version the version to enable
     * @return this builder instance
     */
    public DecoderBuilder enable(CborLdVersion version) {
        enable(versions, version);
        return this;
    }

    /**
     * Disables support for the specified CBOR-LD version.
     *
     * @param version the version to disable
     * @return this builder instance
     */
    public DecoderBuilder disable(CborLdVersion version) {
        versions.remove(version);
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

    /**
     * Enables or disables array compaction for a specific CBOR-LD version.
     *
     * @param version the CBOR-LD version
     * @param enable  {@code true} to enable array compaction
     * @return this builder instance
     */
    public DecoderBuilder compactArray(CborLdVersion version, boolean enable) {
        versions.get(version).compactArrays = enable;
        return this;
    }

    /**
     * Registers a custom document dictionary to be used for decoding.
     * <p>
     * Applies to the default version.
     *
     * @param dictionary the dictionary to register
     * @return this builder instance
     */
    public DecoderBuilder dictionary(DocumentDictionary dictionary) {
        return dictionary(defaultVersion, dictionary);
    }

    /**
     * Registers a custom document dictionary for a specific CBOR-LD version.
     *
     * @param version    the version to associate the dictionary with
     * @param dictionary the dictionary to register
     * @return this builder instance
     */
    public DecoderBuilder dictionary(CborLdVersion version, DocumentDictionary dictionary) {
        versions.get(version).registry.put(dictionary.code(), dictionary);
        return this;
    }

    /**
     * Builds a {@link Decoder} instance based on the current configuration.
     *
     * @return a fully configured {@code Decoder} instance
     */
    public Decoder build() {

        if (loader == null) {
            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
            ((HttpLoader) loader).fallbackContentType(MediaType.JSON);
        }

        if (bundledContexts) {
            loader = new StaticContextLoader(loader);
        }

        // only one?
        if (versions.size() == 1) {
            return newInstance(versions.values().iterator().next(), loader, base);
        }

        return new MultiDecoder(
                versions.values().stream()
                        .map(c -> newInstance(c, loader, base))
                        .collect(Collectors.toUnmodifiableMap(
                                t -> t.config().version(),
                                Function.identity())),
                loader, base);
    }

    /**
     * Builds a {@link DebugDecoder} instance for diagnostic purposes.
     *
     * @return a {@code DebugDecoder} instance
     */
    public DebugDecoder debug() {

        if (loader == null) {
            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
            ((HttpLoader) loader).fallbackContentType(MediaType.JSON);
        }

        if (bundledContexts) {
            loader = new StaticContextLoader(loader);
        }

        return new DebugDecoder(
                versions.values().stream()
                        .collect(Collectors.toUnmodifiableMap(
                                DecoderConfig::version,
                                Function.identity())),
                loader, base);
    }

    protected static final void enable(Map<CborLdVersion, DecoderConfigBuilder> decoders, CborLdVersion version) {
        switch (version) {
        case V1:
            decoders.put(version, DecoderConfigBuilder.of(ConfigV1.INSTANCE));
            break;
        case V06:
            decoders.put(version, DecoderConfigBuilder.of(LegacyConfigV06.INSTANCE));
            break;
        case V05:
            decoders.put(version, DecoderConfigBuilder.of(LegacyConfigV05.INSTANCE));
            break;
        }
    }

    protected static final Decoder newInstance(DecoderConfig config, DocumentLoader loader, URI base) {
        return newInstance(config, config.decoderMapping(), loader, base);
    }

    public static final Decoder newInstance(DecoderConfig config, DecoderMappingProvider mapping, DocumentLoader loader, URI base) {
        switch (config.version()) {
        case V1:
            return new DecoderV1(config, mapping, loader, base);
        case V06:
            return new LegacyDecoderV06(config, mapping, loader, base);
        case V05:
            return new LegacyDecoderV05(config, mapping, loader, base);
        }
        throw new IllegalStateException();
    }
}
