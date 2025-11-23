package com.apicatalog.cborld.decoder;

import java.net.URI;

import com.apicatalog.cborld.CborLd.Version;
import com.apicatalog.cborld.decoder.DecoderException.DecoderCode;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.registry.LegacyDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

class LegacyDecoderV05 extends AbstractDecoder {

    public LegacyDecoderV05(DecoderConfig config, DecoderMappingProvider mapping, DocumentLoader loader, URI base) {
        super(config, mapping, loader, base);
    }

    @Override
    public Object decode(Version version, byte[] encoded) throws DecoderException {
        if (encoded[2] == UNCOMPRESSED_BYTE) {
            throw new DecoderException(DecoderCode.UNSUPPORTED, "Uncompressed CBOR-LD v0.5 is not supported.");
        }
        if (encoded[2] != COMPRESSED_BYTE) {
            throw new DecoderException(DecoderCode.UNSUPPORTED, "Custom dictionaries are not supported by v0.5. Use version 1.0.");
        }

        return decode(LegacyDictionary.DICTIONARY, encoded);
    }
}
