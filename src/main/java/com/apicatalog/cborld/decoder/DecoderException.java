package com.apicatalog.cborld.decoder;

/**
 * Exception thrown when an error occurs during CBOR-LD decoding.
 *
 * <p>
 * A {@code DecoderException} includes a specific {@link DecoderCode} to indicate the
 * category or nature of the decoding failure.
 * </p>
 */
public class DecoderException extends Exception {

    private static final long serialVersionUID = -3475004657414107011L;

    /**
     * Enumeration of known error categories that may occur during decoding.
     */
    public enum DecoderCode {
        /** An unexpected internal error occurred. */
        Internal,
        /** The input CBOR-LD document is invalid or malformed. */
        InvalidDocument,
        /** JSON-LD context to expand the input document is invalid. */
        InvalidContext,
        /** The dictionary code in the document is not recognized or registered. */
        UnknownDictionary,
        /** The document uses features or structures not supported by the decoder. */
        Unsupported,
    }

    private final DecoderCode code;

    /**
     * Constructs a new {@code DecoderException} with the specified error code and
     * message.
     *
     * @param code    the error category
     * @param message a detailed error message
     */
    public DecoderException(DecoderCode code, String message) {
        super(message);
        this.code = code;
    }

    /**
     * Constructs a new {@code DecoderException} with the specified error code and
     * cause.
     *
     * @param code  the error category
     * @param cause the underlying cause of the exception
     */
    public DecoderException(DecoderCode code, Throwable cause) {
        super(cause);
        this.code = code;
    }

    /**
     * Returns the {@link DecoderCode} associated with this exception.
     *
     * @return the error code
     */
    public DecoderCode code() {
        return code;
    }
}
