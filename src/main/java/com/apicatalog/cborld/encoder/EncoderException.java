package com.apicatalog.cborld.encoder;

/**
 * Exception thrown when an error occurs during CBOR-LD encoding.
 *
 * <p>
 * The {@code EncoderException} provides additional context through the
 * {@link Code} enum, which indicates the specific type of failure (e.g. invalid
 * input, unsupported operation, or non-compressible structure).
 * </p>
 */
public class EncoderException extends Exception {

    private static final long serialVersionUID = 4949655193741388758L;

    /**
     * Error codes indicating the reason for an {@link EncoderException}.
     */
    public enum Code {
        /** An unexpected internal error occurred. */
        Internal,

        /** The input JSON-LD document is invalid. */
        InvalidDocument,

        /**
         * The document cannot be compressed, e.g., it contains inline contexts which
         * are not suitable for dictionary-based compression.
         */
        NonCompressible,

        /** The operation is not supported. */
        Unsupported,
    }

    private final Code code;

    /**
     * Constructs a new {@code EncoderException} with the specified error code and
     * message.
     *
     * @param code    the error code indicating the type of failure
     * @param message a human-readable error message
     */
    public EncoderException(Code code, String message) {
        super(message);
        this.code = code;
    }

    /**
     * Constructs a new {@code EncoderException} with the specified error code and
     * cause.
     *
     * @param code  the error code indicating the type of failure
     * @param cause the underlying cause of the exception
     */
    public EncoderException(Code code, Throwable cause) {
        super(cause);
        this.code = code;
    }

    /**
     * Returns the specific error code associated with this exception.
     *
     * @return the {@link Code} representing the cause of the error
     */
    public Code code() {
        return code;
    }
}
