package com.apicatalog.cborld.decoder;

import java.net.URI;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.registry.LegacyDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

import jakarta.json.JsonValue;

public class LegacyDecoderV05 extends BaseDecoder {

    public LegacyDecoderV05(DecoderConfig config, DocumentLoader loader, URI base) {
        super(config, loader, base);
    }

    @Override
    public JsonValue decode(CborLdVersion version, byte[] encoded) throws ContextError, DecoderError {
        return decode(encoded, LegacyDictionary.DICTIONARY);
    }
}
