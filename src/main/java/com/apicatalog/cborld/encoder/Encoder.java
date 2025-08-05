package com.apicatalog.cborld.encoder;

import java.net.URI;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.jsonld.loader.DocumentLoader;

import jakarta.json.JsonObject;

public interface Encoder {

    /**
     * Encodes JSON-LD document as CBOR-LD document.
     * 
     * @param document JSON-LD document to encode
     * @return a byte array representing the encoded CBOR-LD document.
     * 
     * @throws EncoderError
     * @throws ContextError
     */
    byte[] encode(JsonObject document) throws EncoderError, ContextError;

    URI base();

    DocumentLoader loader();

    EncoderConfig config();
}
