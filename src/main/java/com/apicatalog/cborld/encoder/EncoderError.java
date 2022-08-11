package com.apicatalog.cborld.encoder;

public class EncoderError extends Exception {

    private static final long serialVersionUID = -5385517008580552451L;

    public enum Code {
        Internal,           // internal - an unexpected error
        InvalidDocument,    // invalid JSON-LD document
    }

    protected final Code code;

    public EncoderError(Code code, String message) {
        super(message);
        this.code = code;
    }

    public EncoderError(Code code, Throwable cause) {
        super(cause);
        this.code = code;
    }

    public Code getCode() {
        return code;
    }
}
