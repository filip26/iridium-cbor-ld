package com.apicatalog.cborld.decoder;

import java.net.URI;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.Map;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.CborLd.Version;
import com.apicatalog.cborld.decoder.DecoderException.DecoderCode;
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
     * @throws DecoderException if the version is unsupported or if a decoding error
     *                          occurs
     */
    Object decode(byte[] encoded) throws DecoderException;

    /**
     * Decodes a CBOR-LD document into a JSON-LD document using the specified
     * version, without performing document format version validation.
     * <p>
     * This method assumes the provided version matches the document's actual format
     * version.
     *
     * @param version the {@link Version} of the encoded document
     * @param encoded the CBOR-LD encoded document as a byte array
     * @return the decoded JSON-LD document
     * @throws DecoderException if a decoding error occurs
     */
    Object decode(Version version, byte[] encoded) throws DecoderException;

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


    /**
     * Creates a new {@code DecoderBuilder} instance with the given CBOR-LD format
     * versions enabled.
     *
     * @param versions one or more CBOR-LD versions to support
     * @return a new {@code DecoderBuilder} instance
     * @throws IllegalArgumentException if {@code versions} is {@code null} or empty
     */
    public static DecoderBuilder newBuilder(Version... versions) {
        if (versions == null || versions.length == 0) {
            throw new IllegalArgumentException();
        }

        final Map<Version, DecoderConfigBuilder> decoders = new EnumMap<>(Version.class);
        for (Version version : versions) {
            DecoderBuilder.enable(decoders, version);
        }

        return new DecoderBuilder(versions[0], decoders);
    }

    /**
     * Creates a new {@code DecoderBuilder} instance pre-configured with the given
     * decoder configurations.
     *
     * @param configs one or more decoder configurations
     * @return a new {@code DecoderBuilder} instance
     * @throws IllegalArgumentException if {@code configs} is {@code null} or empty
     */
    public static DecoderBuilder newBuilder(DecoderConfig... configs) {
        if (configs == null || configs.length == 0) {
            throw new IllegalArgumentException();
        }

        final Map<Version, DecoderConfigBuilder> decoders = new EnumMap<>(Version.class);
        for (DecoderConfig config : configs) {
            decoders.put(config.version(), DecoderConfigBuilder.of(config));
        }

        return new DecoderBuilder(configs[0].version(), decoders);
    }
    
    static Version assertCborLd(byte[] encoded) throws DecoderException {
        if (encoded == null) {
            throw new IllegalArgumentException("The encoded document paramenter must not be null but byte array.");
        }

        if (encoded.length < 4) {
            throw new DecoderException(DecoderCode.INVALID_DOCUMENT,
                    "The encoded document must be at least 4 bytes but is [" + encoded.length + "].");
        }

        if (encoded[0] != CborLd.LEADING_BYTE) {
            throw new DecoderException(DecoderCode.INVALID_DOCUMENT, "The document is not CBOR-LD document. Must start with "
                    + Hex.toString(CborLd.LEADING_BYTE)
                    + ", but is "
                    + Hex.toString(encoded[0])
                    + ".");
        }

        final Version version = Version.of(encoded, 1); // skip leading byte

        if (version == null) {
            throw new DecoderException(DecoderCode.INVALID_DOCUMENT, "The document is not CBOR-LD document. A tag must start with: "
                    + "v1.0 = "
                    + Hex.toString(Version.V1.bytes())
                    + ", or v0.6 = "
                    + Hex.toString(Version.V06.bytes())
                    + ", or v0.5 = "
                    + Hex.toString(Version.V05.bytes())
                    + ", but is "
                    + Hex.toString(Arrays.copyOfRange(encoded, 1, 3))
                    + ".");
        }

        return version;
    }
}
