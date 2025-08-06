package com.apicatalog.cborld.decoder;

import java.net.URI;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderException.Code;
import com.apicatalog.cborld.registry.LegacyDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

import jakarta.json.JsonValue;

class LegacyDecoderV05 extends BaseDecoder {

    public LegacyDecoderV05(DecoderConfig config, DocumentLoader loader, URI base) {
        super(config, loader, base);
    }

    @Override
    public JsonValue decode(CborLdVersion version, byte[] encoded) throws ContextError, DecoderException {
        if (encoded[2] == CborLd.UNCOMPRESSED_BYTE) {
            throw new DecoderException(Code.Unsupported, "Uncompressed CBOR-LD v0.5 is not supported.");
        }
        if (encoded[2] != CborLd.COMPRESSED_BYTE) {
            throw new DecoderException(Code.Unsupported, "Custom dictionaries are not supported by v0.5. Use version 1.0.");
        }

        return decode(LegacyDictionary.DICTIONARY, encoded);
    }
}
