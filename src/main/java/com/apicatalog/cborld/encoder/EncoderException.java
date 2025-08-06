package com.apicatalog.cborld.encoder;

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

        /** The document cannot be compressed (e.g., it contains inline contexts). */
        NonCompressible,

        /** The operation is not supported. */
        Unsupported,
    }

    private final Code code;

    public EncoderException(Code code, String message) {
        super(message);
        this.code = code;
    }

    public EncoderException(Code code, Throwable cause) {
        super(cause);
        this.code = code;
    }

    public Code code() {
        return code;
    }
}
