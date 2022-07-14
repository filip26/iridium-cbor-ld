package com.apicatalog.cborld;

import com.apicatalog.json.cursor.JsonObjectCursor;

public final class CborLd {

    static final byte[] CBOR_LD_BYTE_PREFIX = new byte[] { (byte)0xD9, 0x05 };
    static final byte UNCOMPRESSED = 0x00;
    static final byte COMPRESSED =  0x01;
    
    public static final Encoder encoder(JsonObjectCursor document) throws EncoderError {
	return Encoder.create(document);
    }

    public static final Decoder decoder(byte[] encodedDocument) throws DecoderError {
	return Decoder.create(encodedDocument);
    }
}
