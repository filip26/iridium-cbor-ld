package com.apicatalog.cborld.decoder;

public class DecoderError extends Throwable {

    private static final long serialVersionUID = -2025187747774695053L;

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
