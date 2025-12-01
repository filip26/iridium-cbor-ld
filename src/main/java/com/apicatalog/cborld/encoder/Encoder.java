package com.apicatalog.cborld.encoder;

import java.net.URI;
import java.util.Map;

import com.apicatalog.cborld.CborLd.Version;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.java.NativeAdapter;

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
     * @param node    the JSON-LD document to encode
     * @param adapter
     * @return a byte array representing the encoded CBOR-LD document
     * @throws EncoderException if encoding fails due to an internal or semantic
     *                          error
     */
    byte[] encode(Object node, TreeAdapter adapter) throws EncoderException;

    default byte[] encode(TreeIO document) throws EncoderException {
        return encode(document.node(), document.adapter());
    }

    default byte[] encode(Map<String, Object> document) throws EncoderException {
        return encode(document, NativeAdapter.instance());
    }

    /**
     * Returns the base URI used for relative URI resolution during encoding.
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
    
    /**
     * Creates a new {@code EncoderBuilder} from an existing {@link EncoderConfig}.
     *
     * @param config the configuration to use
     * @return a new {@code EncoderBuilder} instance
     */
    public static EncoderBuilder newBuilder(EncoderConfig config) {
        return new EncoderBuilder(config);
    }

    /**
     * Creates a new {@code EncoderBuilder} for the given CBOR-LD version.
     *
     * @param version the CBOR-LD version
     * @return a new {@code EncoderBuilder} instance
     */
    public static EncoderBuilder newBuilder(Version version) {
        return new EncoderBuilder(EncoderBuilder.config(version));
    }
}
