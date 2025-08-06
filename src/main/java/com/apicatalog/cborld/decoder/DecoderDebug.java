package com.apicatalog.cborld.decoder;

import java.net.URI;
import java.util.Map;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.model.DataItem;
import jakarta.json.JsonValue;

public class DecoderDebug {

    protected final Map<CborLdVersion, Decoder> decoders;
    protected final DocumentLoader loader;
    protected final URI base;

    protected Decoder decoder;
    protected CborLdVersion version;
    protected DocumentDictionary dictionary;
    protected Mapping mapping;
    protected JsonValue decoded;

    protected Exception error;

    public DecoderDebug(Map<CborLdVersion, Decoder> decoders, DocumentLoader loader, URI base) {
        this.decoders = decoders;
        this.loader = loader;
        this.base = base;

        this.decoder = null;
        this.version = null;
        this.dictionary = null;
        this.decoded = null;
        this.mapping = null;
    }

    public void decode(byte[] encoded) {

        try {
            version = BaseDecoder.assertCborLd(encoded);

            decoder = decoders.get(version);

            if (decoder == null) {
                throw new DecoderError(Code.Unsupported, "The decoder is not configured to support version " + version + " but " + decoders.keySet() + ".");
            }

            var debugger = new Debugger(decoder, this);

            decoded = debugger.decode(encoded);

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

    public DocumentDictionary dictionary() {
        return dictionary;
    }

    public boolean isCborLd() {
        return version != null && dictionary != null;
    }

    public boolean isError() {
        return error != null;
    }

    public Exception error() {
        return error;
    }

    class Debugger extends BaseDecoder {

        Decoder decoder;
        DecoderDebug debug;

        public Debugger(Decoder decoder, DecoderDebug debug) {
            super(decoder.config(), decoder.loader(), decoder.base());
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
        protected Mapping getMapping(DocumentDictionary dictionary, DataItem data) throws DecoderError, ContextError {
            debug.dictionary = dictionary;
            debug.mapping = super.getMapping(dictionary, data);
            return debug.mapping;
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
