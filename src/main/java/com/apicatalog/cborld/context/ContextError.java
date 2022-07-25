package com.apicatalog.cborld.context;

public class ContextError extends Throwable {

    private static final long serialVersionUID = -1522925335270709947L;

    public enum Code {
	InvalidContext,
	UnknownContextCode,
	Unsupported,
    }
    
    protected final Code code;
    
    public ContextError(Code code, String message) {
	super(message);
	this.code = code;
    }

    public ContextError(Code code, Throwable cause) {
	super(cause);
	this.code = code;
    }

    public Code getCode() {
	return code;
    }
}
