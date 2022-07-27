package com.apicatalog.cborld;

import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.encoder.EncoderError;
import com.apicatalog.json.cursor.JsonObjectCursor;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;

public final class CborLd {

    public static final byte[] CBOR_LD_BYTE_PREFIX = new byte[] { (byte)0xD9, 0x05 };
    public static final byte UNCOMPRESSED = 0x00;
    public static final byte COMPRESSED =  0x01;

    public static final Encoder encoder(JsonObjectCursor document) throws EncoderError {
    
        //SchemeRouter.defaultInstance()
        //FIXME
        HttpLoader loader = new HttpLoader(DefaultHttpClient.defaultInstance());
        loader.setFallbackContentType(MediaType.JSON);
    
        return encoder(document, loader);
    }

    public static final Decoder decoder(byte[] encodedDocument) throws DecoderError {
    
        //SchemeRouter.defaultInstance()
        //FIXME
        HttpLoader loader = new HttpLoader(DefaultHttpClient.defaultInstance());
        loader.setFallbackContentType(MediaType.JSON);
    
        return decoder(encodedDocument, loader);
        }
    
        public static final Encoder encoder(JsonObjectCursor document, DocumentLoader loader) throws EncoderError {
        return Encoder.create(document, loader);
        }
    
        public static final Decoder decoder(byte[] encodedDocument, DocumentLoader loader) throws DecoderError {
        return Decoder.create(encodedDocument, loader);
    }
}
