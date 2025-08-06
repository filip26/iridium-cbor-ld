package com.apicatalog.cborld;

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
