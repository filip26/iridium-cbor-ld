package com.apicatalog.cborld;

public class EncoderError extends Throwable {

    private static final long serialVersionUID = -2025187747774695053L;

    public enum Code {
	InvalidDocument,	// invalid JSON-LD document
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
