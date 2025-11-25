package com.apicatalog.cborld.decoder;

/**
 * Exception thrown when an error occurs during CBOR-LD decoding.
 *
 * <p>
 * A {@code DecoderException} includes a specific {@link DecoderError} to indicate the
 * category or nature of the decoding failure.
 * </p>
 */
public class DecoderException extends Exception {

    private static final long serialVersionUID = -3475004657414107011L;

    /**
     * Enumeration of known error categories that may occur during decoding.
     */
    public enum DecoderError {
        /** An unexpected internal error occurred. */
        INTERNAL,
        /** The input CBOR-LD document is invalid or malformed. */
        INVALID_DOCUMENT,
        /** JSON-LD context to expand the input document is invalid. */
        INVALID_CONTEXT,
        /** The dictionary code in the document is not recognized or registered. */
        UNKNOWN_DICTIONARY,
        /** The document uses features or structures not supported by the decoder. */
        UNSUPPORTED, 
        
        INVALID_VALUE,
    }

    private final DecoderError code;

    /**
     * Constructs a new {@code DecoderException} with the specified error code and
     * message.
     *
     * @param code    the error category
     * @param message a detailed error message
     */
    public DecoderException(DecoderError code, String message) {
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
    public DecoderException(DecoderError code, Throwable cause) {
        super(cause);
        this.code = code;
    }

    /**
     * Returns the {@link DecoderError} associated with this exception.
     *
     * @return the error code
     */
    public DecoderError code() {
        return code;
    }
}
