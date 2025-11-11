package com.apicatalog.cborld.decoder;

import java.net.URI;
import java.util.Arrays;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderException.Code;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.jsonld.loader.DocumentLoader;

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
     * @return the decoded JSON-LD document
     * @throws ContextError     if a context-related error occurs during decoding
     * @throws DecoderException if the version is unsupported or if a decoding error
     *                          occurs
     */
    Object decode(byte[] encoded) throws ContextError, DecoderException;

    /**
     * Decodes a CBOR-LD document into a JSON-LD document using the specified
     * version, without performing document format version validation.
     * <p>
     * This method assumes the provided version matches the document's actual format
     * version.
     *
     * @param version the {@link CborLdVersion} of the encoded document
     * @param encoded the CBOR-LD encoded document as a byte array
     * @return the decoded JSON-LD document
     * @throws ContextError     if a context-related error occurs during decoding
     * @throws DecoderException if a decoding error occurs
     */
    Object decode(CborLdVersion version, byte[] encoded) throws ContextError, DecoderException;

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
    
    static CborLdVersion assertCborLd(byte[] encoded) throws DecoderException {
        if (encoded == null) {
            throw new IllegalArgumentException("The encoded document paramenter must not be null but byte array.");
        }

        if (encoded.length < 4) {
            throw new DecoderException(Code.InvalidDocument,
                    "The encoded document must be at least 4 bytes but is [" + encoded.length + "].");
        }

        if (encoded[0] != CborLd.LEADING_BYTE) {
            throw new DecoderException(Code.InvalidDocument, "The document is not CBOR-LD document. Must start with "
                    + Hex.toString(CborLd.LEADING_BYTE)
                    + ", but is "
                    + Hex.toString(encoded[0])
                    + ".");
        }

        final CborLdVersion version = CborLdVersion.of(encoded, 1); // skip leading byte

        if (version == null) {
            throw new DecoderException(Code.InvalidDocument, "The document is not CBOR-LD document. A tag must start with: "
                    + "v1.0 = "
                    + Hex.toString(CborLdVersion.V1.bytes())
                    + ", or v0.6 = "
                    + Hex.toString(CborLdVersion.V06.bytes())
                    + ", or v0.5 = "
                    + Hex.toString(CborLdVersion.V05.bytes())
                    + ", but is "
                    + Hex.toString(Arrays.copyOfRange(encoded, 1, 3))
                    + ".");
        }

        return version;
    }
}
