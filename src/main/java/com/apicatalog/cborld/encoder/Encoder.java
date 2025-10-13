package com.apicatalog.cborld.encoder;

import java.net.URI;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.tree.io.NodeAdapter;

/**
 * Interface for encoding JSON-LD documents into CBOR-LD format.
 *
 * <p>
 * Implementations of this interface transform JSON-LD into compact binary
 * CBOR-LD representations, using dictionary compression and context
 * optimization.
 * </p>
 *
 * <p>
 * Encoders are typically created via {@code CborLd.createEncoder()} and
 * configured through the {@link EncoderBuilder}.
 * </p>
 */
public interface Encoder {

    /**
     * Encodes the given JSON-LD document into CBOR-LD format.
     *
     * @param document the JSON-LD document to encode
     * @param adapter
     * @return a byte array representing the encoded CBOR-LD document
     * @throws EncoderException if encoding fails due to an internal or semantic
     *                          error
     * @throws ContextError     if a context resolution or validation issue occurs
     */
    byte[] encode(Object document, NodeAdapter adapter) throws EncoderException, ContextError;

    /**
     * Returns the base URI used for relative IRI resolution during encoding.
     *
     * @return the base URI, or {@code null} if not set
     */
    URI base();

    /**
     * Returns the {@link DocumentLoader} used to retrieve remote or static JSON-LD
     * contexts.
     *
     * @return the configured document loader
     */
    DocumentLoader loader();

    /**
     * Returns the encoder configuration used for this encoder instance.
     *
     * @return the {@link EncoderConfig}
     */
    EncoderConfig config();
}
