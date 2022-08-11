package com.apicatalog.cborld.decoder;

public class DecoderError extends Exception {

    private static final long serialVersionUID = -482268633978506824L;

    public enum Code {
        Internal,
        InvalidDocument,
        UnknownCompression,
        Unsupported,
    }

    protected final Code code;

    public DecoderError(Code code, String message) {
        super(message);
        this.code = code;
    }

    public DecoderError(Code code, Throwable cause) {
        super(cause);
        this.code = code;
    }

    public Code getCode() {
        return code;
    }
}
