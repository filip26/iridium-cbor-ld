package com.apicatalog.cborld.mapping.context;

public class ContextMappingException extends Exception {

    private static final long serialVersionUID = -1522925335270709947L;

    public enum Code {
        InvalidContext,
        UnknownContextCode,
        Unsupported,
    }

    protected final Code code;

    public ContextMappingException(Code code, String message) {
        super(message);
        this.code = code;
    }

    public ContextMappingException(Code code, Throwable cause) {
        super(cause);
        this.code = code;
    }

    public Code getCode() {
        return code;
    }
}
