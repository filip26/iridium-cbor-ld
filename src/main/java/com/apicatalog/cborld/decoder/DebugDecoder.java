package com.apicatalog.cborld.decoder;

import java.net.URI;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.jsonld.loader.DocumentLoader;

import jakarta.json.JsonValue;

public class DebugDecoder {

    protected final Decoder decoder;

    protected CborLdVersion version;
    
    protected JsonValue decoded;
    
    protected Exception error;

    public DebugDecoder(Decoder decoder) {
        this.decoder = decoder;
        this.version = null;
        this.decoded = null;
    }

    public void decode(byte[] encoded) {

        var debugger = new Debugger(decoder, this);
        
        try {
            this.decoded = debugger.decode(encoded);
            
        } catch (ContextError | DecoderError e) {
            this.error = e;
        }
    }

    public Decoder decoder() {
        return decoder;
    }

    public CborLdVersion version() {
        return version;
    }

    class Debugger implements Decoder {

        Decoder decoder;
        DebugDecoder debug;

        public Debugger(Decoder decoder, DebugDecoder debug) {
            this.decoder = decoder;
        }

        @Override
        public JsonValue decode(byte[] encoded) throws ContextError, DecoderError {
            return decoder.decode(encoded);
        }

        @Override
        public JsonValue decode(CborLdVersion version, byte[] encoded) throws ContextError, DecoderError {
            debug.version = version;
            return decoder.decode(version, encoded);
        }

        @Override
        public DecoderConfig config() {
            return decoder.config();
        }

        @Override
        public URI base() {
            return decoder.base();
        }

        @Override
        public DocumentLoader loader() {
            return decoder.loader();
        }
    }
}
