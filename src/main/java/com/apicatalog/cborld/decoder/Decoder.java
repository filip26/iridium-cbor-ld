package com.apicatalog.cborld.decoder;

import java.net.URI;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.jsonld.loader.DocumentLoader;

import jakarta.json.JsonValue;

/**
 * Interface for decoding CBOR-LD encoded documents into JSON-LD.
 * <p>
 * Implementations should validate the document format version to ensure
 * compatibility with the decoder. If the document uses an unsupported version,
 * a {@link DecoderException} should be thrown.
 */
public interface Decoder {

    /**
     * Decodes a CBOR-LD document into a JSON-LD document.
     * <p>
     * This method will extract and validate the document's version before decoding.
     *
     * @param encoded the CBOR-LD encoded document as a byte array
     * @return the decoded JSON-LD document as a {@link JsonValue}
     * @throws ContextError     if a context-related error occurs during decoding
     * @throws DecoderException if the version is unsupported or if a decoding error
     *                          occurs
     */
    JsonValue decode(byte[] encoded) throws ContextError, DecoderException;

    /**
     * Decodes a CBOR-LD document into a JSON-LD document using the specified
     * version, without performing document format version validation.
     * <p>
     * This method assumes the provided version matches the document's actual format
     * version.
     *
     * @param version the {@link CborLdVersion} of the encoded document
     * @param encoded the CBOR-LD encoded document as a byte array
     * @return the decoded JSON-LD document as a {@link JsonValue}
     * @throws ContextError     if a context-related error occurs during decoding
     * @throws DecoderException if a decoding error occurs
     */
    JsonValue decode(CborLdVersion version, byte[] encoded) throws ContextError, DecoderException;

    /**
     * Returns the decoder configuration in use.
     *
     * @return the {@link DecoderConfig} applied to this decoder
     */
    DecoderConfig config();

    /**
     * Returns the base URI used during decoding.
     *
     * @return the base {@link URI} or {@code null} if not set
     */
    URI base();

    /**
     * Returns the document loader used to resolve external contexts.
     *
     * @return the {@link DocumentLoader} used by this decoder
     */
    DocumentLoader loader();
}
