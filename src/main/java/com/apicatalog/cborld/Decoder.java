package com.apicatalog.cborld;

import java.io.ByteArrayInputStream;
import java.util.List;

import com.apicatalog.cborld.DecoderError.Code;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;
import jakarta.json.JsonValue;

public class Decoder {

    protected final byte[] encoded;
    protected final boolean compressed;
    
    protected Decoder(byte[] encoded, boolean compressed) {
	this.encoded = encoded;
	this.compressed = compressed;
    }
    
    public static final Decoder create(byte[] encodedDocument) throws DecoderError {
	
	if (encodedDocument == null) {
	    throw new IllegalArgumentException("The encoded document paramenter must not be null but byte arrayy.");
	}
	
	if (encodedDocument.length < 4) {
	    throw new DecoderError(Code.InvalidDocument, "The encoded document must be at least 4 bytes but is [" + encodedDocument.length + "].");
	}
	
	if (encodedDocument[0] != CborLd.CBOR_LD_BYTE_PREFIX[0]  || encodedDocument[1] != CborLd.CBOR_LD_BYTE_PREFIX[1]) {
	    throw new DecoderError(Code.InvalidDocument, "The document is not CBOR-LD document.");
	}
	
	if (encodedDocument[2] == CborLd.COMPRESSED) {
	    return new Decoder(encodedDocument, true);
	} 
	
	if (encodedDocument[2] == CborLd.UNCOMPRESSED) {
	    return new Decoder(encodedDocument, false);
	}
	
	throw new DecoderError(Code.UnknownCompression, "Unkknown CBOR-LD document compression, expected 0x00 - uncompressed or 0x01 - compressed, but found [" + Hex.toString(encodedDocument[2]) + "].");
    }

    public JsonValue decode() throws DecoderError {
	if (compressed) {
	    return decodeCompressed(encoded);
	}
	return decodeUncompressed(encoded);
    }
    
    static final JsonValue decodeCompressed(byte[] encoded) throws DecoderError {
	
	try {
	    final ByteArrayInputStream bais = new ByteArrayInputStream(encoded);
	    final List<DataItem> dataItems= new CborDecoder(bais).decode();
	    
	    for(DataItem dataItem : dataItems) {
		System.out.println(dataItem.getTag());
		System.out.println(dataItem.getMajorType());
		System.out.println(dataItem.getOuterTaggable());
		System.out.println(dataItem);
		
		System.out.println(((Map)dataItem).getKeys());
		System.out.println(((Map)dataItem).getValues());
	    }

	    ///TODO
	    return null;

	} catch (CborException e) {
	    throw new DecoderError(Code.InvalidDocument, e);
	}
    }

    static final JsonValue decodeUncompressed(byte[] encoded) {
	///TODO
	return null;
    }
}
