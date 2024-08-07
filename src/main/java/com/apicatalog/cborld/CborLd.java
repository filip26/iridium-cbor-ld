package com.apicatalog.cborld;

import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.decoder.DecoderBuilder;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.encoder.EncoderBuilder;
import com.apicatalog.cborld.encoder.EncoderConfig;

/**
 * High level API to process CBOR-LD.
 */
public class CborLd {

    public static final byte LEADING_BYTE = (byte) 0xD9;

    public static final byte VERSION_5_BYTE = (byte) 0x05;
    public static final byte VERSION_6_BYTE = (byte) 0x06;

    public static final byte UNCOMPRESSED_BYTE = 0x00;
    public static final byte COMPRESSED_BYTE = 0x01;

    private CborLd() {
        /* protected */ }

    /**
     * Create a new {@link DecoderBuilder} allowing to configure a decoder. The
     * builder is initialized by {@link DefaultConfig}.
     * 
     * @return a new {@link DecoderBuilder} instance
     * 
     */
    public static DecoderBuilder createDecoder() {
        return createDecoder(DefaultConfig.INSTANCE);
    }

    /**
     * Create a new {@link DecoderBuilder} allowing to configure a decoder.
     * 
     * @param config an initial configuration
     * @return a new {@link DecoderBuilder} instance
     * 
     */
    public static DecoderBuilder createDecoder(DecoderConfig config) {
        return new DecoderBuilder(config);
    }

    /**
     * Create a new {@link EncoderBuilder} allowing to configure an encoder. The
     * builder is initialized by {@link DefaultConfig}.
     * 
     * @return a new {@link EncoderBuilder} instance
     * 
     */
    public static EncoderBuilder createEncoder() {
        return createEncoder(DefaultConfig.INSTANCE);
    }

    /**
     * Create a new {@link EncoderBuilder} allowing to configure an encoder.
     * 
     * @param config an initial configuration
     * @return a new {@link EncoderBuilder} instance
     * 
     */
    public static EncoderBuilder createEncoder(EncoderConfig config) {
        return new EncoderBuilder(config);
    }
}
