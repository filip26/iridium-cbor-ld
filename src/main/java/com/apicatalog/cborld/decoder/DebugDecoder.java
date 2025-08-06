package com.apicatalog.cborld.decoder;

import java.net.URI;
import java.util.Map;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderException.Code;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.model.DataItem;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

public class DebugDecoder {

    protected final Map<CborLdVersion, DecoderConfig> decoders;
    protected final DocumentLoader loader;
    protected final URI base;

    protected Decoder decoder;
    protected CborLdVersion version;
    protected DocumentDictionary dictionary;
    protected Mapping mapping;
    protected JsonValue decoded;

    protected Exception error;

    public DebugDecoder(Map<CborLdVersion, DecoderConfig> decoders, DocumentLoader loader, URI base) {
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

            var config = decoders.get(version);

            if (config == null) {
                throw new DecoderException(Code.Unsupported, "The decoder is not configured to support version " + version + " but " + decoders.keySet() + ".");
            }

            var debugger = DecoderBuilder.newInstance(
                    config,
                    new DebugMapping(config.decoderMapping(), this),
                    loader,
                    base);

            decoded = debugger.decode(encoded);

        } catch (ContextError | DecoderException e) {
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

    public JsonObject dump() {
        var builder = Json.createObjectBuilder();

        builder.add("version", version != null ? Json.createValue(version.name()) : JsonValue.NULL);
        builder.add("dictionary", dictionary != null ? dump(dictionary) : JsonValue.NULL);
        
        if (mapping != null) {
            builder.add("termMap", dump(mapping.termMap()));
        }
        
        return builder.build();
    }

    static final JsonObject dump(DocumentDictionary dictionary) {
        var builder = Json.createObjectBuilder();
        builder.add("code", dictionary.code());
        builder.add("context", dump(dictionary.contexts()));
        builder.add("uri", dump(dictionary.uris()));
        
        return builder.build();
    }
    
    static final JsonObject dump(Dictionary dictionary) {
        var builder = Json.createObjectBuilder();
        dictionary.iterator().forEachRemaining(e -> builder.add(e.getKey(), e.getValue()));
        return builder.build();        
    }

    class DebugMapping implements DecoderMappingProvider {

        DecoderMappingProvider provider;
        DebugDecoder debug;

        public DebugMapping(DecoderMappingProvider provider, DebugDecoder debug) {
            this.provider = provider;
            this.debug = debug;
        }

        @Override
        public Mapping getDecoderMapping(DataItem document, DocumentDictionary dictionary, Decoder decoder) throws DecoderException, ContextError {
            debug.dictionary = dictionary;
            debug.decoder = decoder;
            debug.mapping = provider.getDecoderMapping(document, dictionary, decoder);
            return debug.mapping;
        }

    }
}
