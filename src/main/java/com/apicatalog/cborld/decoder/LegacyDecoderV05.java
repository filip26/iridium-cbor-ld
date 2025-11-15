package com.apicatalog.cborld.decoder;

import java.net.URI;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.decoder.DecoderException.Code;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.mapping.context.ContextMappingException;
import com.apicatalog.cborld.registry.LegacyDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

class LegacyDecoderV05 extends AbstractDecoder {

    public LegacyDecoderV05(DecoderConfig config, DecoderMappingProvider mapping, DocumentLoader loader, URI base) {
        super(config, mapping, loader, base);
    }

    @Override
    public Object decode(CborLdVersion version, byte[] encoded) throws ContextMappingException, DecoderException {
        if (encoded[2] == UNCOMPRESSED_BYTE) {
            throw new DecoderException(Code.Unsupported, "Uncompressed CBOR-LD v0.5 is not supported.");
        }
        if (encoded[2] != COMPRESSED_BYTE) {
            throw new DecoderException(Code.Unsupported, "Custom dictionaries are not supported by v0.5. Use version 1.0.");
        }

        return decode(LegacyDictionary.DICTIONARY, encoded);
    }
}
