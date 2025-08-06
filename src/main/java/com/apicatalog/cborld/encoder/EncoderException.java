package com.apicatalog.cborld.encoder;

public class EncoderException extends Exception {

    private static final long serialVersionUID = 4949655193741388758L;

    public enum Code {
        Internal, // internal - an unexpected error
        InvalidDocument, // invalid JSON-LD document
        Unsupported,
    }

    protected final Code code;

    public EncoderException(Code code, String message) {
        super(message);
        this.code = code;
    }

    public EncoderException(Code code, Throwable cause) {
        super(cause);
        this.code = code;
    }

    public Code getCode() {
        return code;
    }
}
