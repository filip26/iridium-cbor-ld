package com.apicatalog.cborld;

import java.util.Map;

import com.apicatalog.cborld.config.ConfigV1;
import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.decoder.DecoderBuilder;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.encoder.EncoderBuilder;
import com.apicatalog.cborld.encoder.EncoderConfig;

/**
 * High-level entry point for working with CBOR-LD.
 * <p>
 * Provides static factory methods to configure and create CBOR-LD
 * {@link Encoder} and {@link Decoder} instances.
 */
public class CborLd {

    /** CBOR tag leading byte. */
    public static final byte LEADING_BYTE = (byte) 0xD9;

    /** CBOR-LD version 1 identifier. */
    public static final byte[] VERSION_1_BYTES = new byte[] { (byte) 0xCB, 0x1D };
    /** CBOR-LD legacy version 0.6 identifier. */
    public static final byte VERSION_06_BYTE = (byte) 0x06;
    /** CBOR-LD legacy version 0.5 identifier. */
    public static final byte VERSION_05_BYTE = (byte) 0x05;

    public static final Map<String, String> CONTEXT_RESOURCES = Map.ofEntries(
            Map.entry(
                    "https://www.w3.org/2018/credentials/examples/v1",
                    "/com/apicatalog/cborld/loader/2018-credentials-examples-v1.jsonld"),
            Map.entry(
                    "https://www.w3.org/2018/credentials/v1",
                    "/com/apicatalog/cborld/loader/2018-credentials-v1.jsonld"),
            Map.entry(
                    "https://w3id.org/security/suites/ed25519-2020/v1",
                    "/com/apicatalog/cborld/loader/security-suites-ed25519-2020-v1.jsonld"),
            Map.entry(
                    "https://www.w3.org/ns/odrl.jsonld",
                    "/com/apicatalog/cborld/loader/odrl.jsonld"),
            Map.entry(
                    "https://www.w3.org/ns/did/v1",
                    "/com/apicatalog/cborld/loader/did-v1.jsonld"),
            Map.entry(
                    "https://www.w3.org/ns/credentials/v2",
                    "/com/apicatalog/cborld/loader/credentials-v2.jsonld"),
            Map.entry(
                    "https://www.w3.org/ns/cid/v1",
                    "/com/apicatalog/cborld/loader/cid-v1.jsonld"),
            Map.entry(
                    "https://w3id.org/vc-barcodes/v1",
                    "/com/apicatalog/cborld/loader/vc-barcodes-v1.jsonld"),
            Map.entry(
                    "https://w3id.org/security/data-integrity/v1",
                    "/com/apicatalog/cborld/loader/data-integrity-v1.jsonld"),
            Map.entry(
                    "https://w3id.org/security/multikey/v1",
                    "/com/apicatalog/cborld/loader/multikey-v1.jsonld"),
            Map.entry(
                    "https://w3id.org/security/data-integrity/v2",
                    "/com/apicatalog/cborld/loader/data-integrity-v2.jsonld"),
            Map.entry(
                    "https://www.w3.org/ns/credentials/examples/v2",
                    "/com/apicatalog/cborld/loader/credentials-examples-v2.jsonld"),
            Map.entry(
                    "https://www.w3.org/ns/activitystreams",
                    "/com/apicatalog/cborld/loader/activitystreams.jsonld"));

    /** Utility class — not meant to be instantiated. */
    protected CborLd() {
        /* protected */ }

    /**
     * Creates a new {@link DecoderBuilder} instance with default configuration.
     * <p>
     * The builder is initialized with default settings provided by
     * {@link ConfigV1}.
     *
     * @return a new {@link DecoderBuilder} instance
     */
    public static DecoderBuilder createDecoder() {
        return createDecoder(ConfigV1.INSTANCE);
    }

    /**
     * Creates a new {@link DecoderBuilder} instance using the provided decoder
     * configuration(s).
     * <p>
     * This allows customization of decoding behavior by specifying one or more
     * {@link DecoderConfig} options.
     *
     * @param config one or more decoder configurations
     * @return a new {@link DecoderBuilder} instance
     */
    public static DecoderBuilder createDecoder(DecoderConfig... config) {
        return DecoderBuilder.of(config);
    }

    /**
     * Creates a new {@link DecoderBuilder} instance for the specified CBOR-LD
     * format version(s).
     * <p>
     * Use this method to explicitly support only certain {@link CborLdVersion}s
     * during decoding.
     *
     * @param version one or more supported CBOR-LD format versions
     * @return a new {@link DecoderBuilder} instance
     */
    public static DecoderBuilder createDecoder(CborLdVersion... version) {
        return DecoderBuilder.of(version);
    }

    /**
     * Creates a new {@link EncoderBuilder} instance using the default
     * configuration.
     * <p>
     * The builder is initialized with default settings provided by
     * {@link ConfigV1}.
     *
     * @return a new {@link EncoderBuilder} instance
     */
    public static EncoderBuilder createEncoder() {
        return createEncoder(ConfigV1.INSTANCE);
    }

    /**
     * Creates a new {@link EncoderBuilder} instance with a custom configuration.
     *
     * @param config the encoder configuration to apply
     * @return a new {@link EncoderBuilder} instance
     */
    public static EncoderBuilder createEncoder(EncoderConfig config) {
        return EncoderBuilder.of(config);
    }

    /**
     * Creates a new {@link EncoderBuilder} instance for the specified CBOR-LD
     * format version.
     *
     * @param version the CBOR-LD version to use
     * @return a new {@link EncoderBuilder} instance
     */
    public static EncoderBuilder createEncoder(CborLdVersion version) {
        return EncoderBuilder.of(version);
    }
}
