package com.apicatalog.cborld;

import com.apicatalog.json.cursor.JsonObjectCursor;

import jakarta.json.JsonValue;

public final class CborLd {

    static final byte[] CBOR_LD_BYTE_PREFIX = new byte[] { (byte)0xD9, 0x05 };
    static final byte UNCOMPRESSED = 0x00;
    static final byte COMPRESSED =  0x01;
    
    //FIXME return EncoderApi
    public static final byte[] encode(JsonObjectCursor document) throws EncoderError {
	return Encoder.encode(document);
    }

    //FIXME return DecoderApi
    public static final JsonValue decode(byte[] encodedDocument) throws DecoderError {
	return Decoder.decode(encodedDocument);
    }
}
