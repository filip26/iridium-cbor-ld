package com.apicatalog.cborld.debug;

import java.net.URI;
import java.util.Map;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.BaseDecoder;
import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.decoder.DecoderBuilder;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.decoder.DecoderException.Code;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.model.DataItem;

public class DebugDecoder extends CborLdDebug {

    final Map<CborLdVersion, DecoderConfig> versions;

    public DebugDecoder(Map<CborLdVersion, DecoderConfig> versions, DocumentLoader loader, URI base) {
        super(loader, base);
        this.versions = versions;
    }

    public void decode(byte[] encoded) {
        try {
            version = BaseDecoder.assertCborLd(encoded);

            var config = versions.get(version);

            if (config == null) {
                throw new DecoderException(Code.Unsupported, "The decoder is not configured to support version " + version + " but " + versions.keySet() + ".");
            }

            var debug = DecoderBuilder.newInstance(
                    config,
                    new DebugMapping(config.decoderMapping(), this),
                    loader,
                    base);

            decoded = debug.decode(encoded);

        } catch (ContextError | DecoderException e) {
            this.error = e;
        }
    }

    class DebugMapping implements DecoderMappingProvider {

        final DecoderMappingProvider provider;
        final CborLdDebug debug;

        public DebugMapping(DecoderMappingProvider provider, CborLdDebug debug) {
            this.provider = provider;
            this.debug = debug;
        }

        @Override
        public Mapping getDecoderMapping(DataItem document, DocumentDictionary dictionary, Decoder decoder) throws DecoderException, ContextError {
            debug.dictionary = dictionary;
            debug.mapping = provider.getDecoderMapping(document, dictionary, decoder);
            return debug.mapping;
        }
    }
}
