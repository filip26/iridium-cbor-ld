package com.apicatalog.cborld;

import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.decoder.DecoderBuilder;
import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.encoder.EncoderBuilder;
import com.apicatalog.cborld.encoder.EncoderError;

/**
 * High level API to process CBOR-LD.
 */
public class CborLd {

    public static final byte LEADING_BYTE = (byte)0xD9;
    
    public static final byte VERSION_5_BYTE = (byte)0x05;
    public static final byte VERSION_6_BYTE = (byte)0x06;
    
    public static final byte UNCOMPRESSED_BYTE = 0x00;
    public static final byte COMPRESSED_BYTE =  0x01;


    /**
     * Decodes CBOR-LD document as JSON-LD document.
     * 
     * @param document CBOR-LD document to decode
     * @return a new {@link DecoderBuilder} instance allowing to decode the given document
     * 
     * @throws DecoderError
     */
    public static DecoderBuilder createDecoder() {
        return new DecoderBuilder(DefaultConfig.INSTANCE);
    }

    /** 
     * Encodes JSON-LD document as CBOR-LD document.
     * 
     * @param document JSON-LD document to encode 
     * @return a new {@link EncoderBuilder} instance allowing to encode the given document
     * 
     * @throws EncoderError
     */
    public static EncoderBuilder createEncoder() {
        return new EncoderBuilder(DefaultConfig.INSTANCE);
    }
}
