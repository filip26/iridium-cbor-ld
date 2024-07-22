package com.apicatalog.cborld;

import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.encoder.EncoderError;
import com.apicatalog.cursor.jakarta.JakartaJsonCursor;

import jakarta.json.JsonObject;

/**
 * High level API to process CBOR-LD.
 */
public final class CborLd {

    public static final byte CBOR_LD_LEADING_BYTE = (byte)0xD9;
    
    public static final byte CBOR_LD_VERSION_5_BYTE = (byte)0x05;
    public static final byte CBOR_LD_VERSION_6_BYTE = (byte)0x06;
    
    public static final byte UNCOMPRESSED = 0x00;
    public static final byte COMPRESSED_V1 =  0x01;

    /**
     * Encodes JSON-LD document as CBOR-LD document.
     * 
     * @param document JSON-LD document to encode 
     * @return a new {@link Encoder} instance allowing to encode the given document
     * 
     * @throws EncoderError
     */
    public static final Encoder encoder(JsonObject document) throws EncoderError {
        return Encoder.create(JakartaJsonCursor.from(document));
    }

    /**
     * Decodes CBOR-LD document as JSON-LD document.
     * 
     * @param document CBOR-LD document to decode
     * @return a new {@link Decoder} instance allowing to decode the given document
     * 
     * @throws DecoderError
     */
    public static final Decoder decoder(byte[] document) throws DecoderError {
        return Decoder.create(document);
    }
}
