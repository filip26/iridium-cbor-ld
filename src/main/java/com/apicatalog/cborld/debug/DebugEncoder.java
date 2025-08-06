package com.apicatalog.cborld.debug;

import java.net.URI;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.encoder.DefaultEncoder;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.encoder.EncoderConfig;
import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.loader.DocumentLoader;

import jakarta.json.JsonObject;

public class DebugEncoder extends CborLdDebug {

    final EncoderConfig config;

    public DebugEncoder(EncoderConfig config, DocumentLoader loader, URI base) {
        super(loader, base);
        this.config = config;
    }

    public void encode(JsonObject object) {
        try {

            var debug = new DefaultEncoder(
                    config,
                    new DebugMapping(config.encoderMapping(), this),
                    loader,
                    base);

            version = config.version();
            dictionary = config.dictionary();
            encoded = debug.encode(object);

        } catch (ContextError | EncoderException e) {
            this.error = e;
        }
    }

    class DebugMapping implements EncoderMappingProvider {

        final EncoderMappingProvider provider;
        final CborLdDebug debug;

        public DebugMapping(EncoderMappingProvider provider, CborLdDebug debug) {
            this.provider = provider;
            this.debug = debug;
        }

        @Override
        public Mapping getEncoderMapping(JsonObject document, Encoder encoder) throws ContextError {
            debug.mapping = provider.getEncoderMapping(document, encoder);
            return debug.mapping;
        }
    }
}
