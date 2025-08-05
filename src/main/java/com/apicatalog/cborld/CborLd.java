package com.apicatalog.cborld;

import com.apicatalog.cborld.config.ConfigV1;
import com.apicatalog.cborld.decoder.DecoderBuilder;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.encoder.EncoderBuilder;
import com.apicatalog.cborld.encoder.EncoderConfig;

/**
 * High level API to process CBOR-LD.
 */
public class CborLd {

    public static final byte LEADING_BYTE = (byte) 0xD9; // tag

    public static final byte[] VERSION_1_BYTES = new byte[] { (byte) 0xCB, 0x1D };
    public static final byte VERSION_06_BYTE = (byte) 0x06;
    public static final byte VERSION_05_BYTE = (byte) 0x05;

    public static final byte UNCOMPRESSED_BYTE = 0x00;

    public static final byte COMPRESSED_BYTE = 0x01;

    protected CborLd() {
        /* protected */ }

    /**
     * Creates a new {@link DecoderBuilder} instance with default configuration.
     * <p>
     * The builder is initialized with all available format version decoders.
     *
     * @return a new {@link DecoderBuilder} instance
     */
    public static DecoderBuilder createDecoder() {
        return createDecoder(ConfigV1.INSTANCE);
    }

    /**
     * Creates a new {@link DecoderBuilder} instance with the specified decoder
     * configurations.
     * <p>
     * This method allows initializing the builder with one or more custom
     * {@link DecoderConfig} options to control decoding behavior.
     *
     * @param config one or more initial decoder configurations
     * @return a new {@link DecoderBuilder} instance
     */
    public static DecoderBuilder createDecoder(DecoderConfig... config) {
        return DecoderBuilder.of(config);
    }

    /**
     * Creates a new {@link DecoderBuilder} instance for the specified CBOR-LD
     * format version(s).
     * <p>
     * This method allows initializing the builder with support for specific
     * {@link CborLdVersion}s only.
     *
     * @param version one or more supported CBOR-LD format versions
     * @return a new {@link DecoderBuilder} instance
     */
    public static DecoderBuilder createDecoder(CborLdVersion... version) {
        return DecoderBuilder.of(version);
    }

    /**
     * Create a new {@link EncoderBuilder} allowing to configure an encoder. The
     * builder is initialized by {@link ConfigV1}.
     * 
     * @return a new {@link EncoderBuilder} instance
     * 
     */
    public static EncoderBuilder createEncoder() {
        return createEncoder(ConfigV1.INSTANCE);
    }

    /**
     * Create a new {@link EncoderBuilder} allowing to configure an encoder.
     * 
     * @param config an initial configuration
     * @return a new {@link EncoderBuilder} instance
     * 
     */
    public static EncoderBuilder createEncoder(EncoderConfig config) {
        return EncoderBuilder.of(config);
    }
    
    public static EncoderBuilder createEncoder(CborLdVersion version) {
        return EncoderBuilder.of(version);
    }
}
