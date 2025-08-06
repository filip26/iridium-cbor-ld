package com.apicatalog.cborld.decoder;

public class DecoderException extends Exception {

    private static final long serialVersionUID = -3475004657414107011L;

    public enum Code {
        Internal, // internal - an unexpected error
        InvalidDocument, // an invalid CBOR-LD document
        UnknownDictionary,
        Unsupported,
    }

    private final Code code;

    public DecoderException(Code code, String message) {
        super(message);
        this.code = code;
    }

    public DecoderException(Code code, Throwable cause) {
        super(cause);
        this.code = code;
    }

    public Code code() {
        return code;
    }
}
