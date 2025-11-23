package com.apicatalog.cborld.decoder;

import java.net.URI;

import com.apicatalog.cborld.CborLd.Version;
import com.apicatalog.cborld.decoder.DecoderException.DecoderCode;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

class LegacyDecoderV06 extends AbstractDecoder {

    public LegacyDecoderV06(DecoderConfig config, DecoderMappingProvider mapping, DocumentLoader loader, URI base) {
        super(config, mapping, loader, base);
    }

    @Override
    public Object decode(Version version, byte[] encoded) throws DecoderException {

        if (encoded[2] == UNCOMPRESSED_BYTE) {
            throw new DecoderException(DecoderCode.UNSUPPORTED, "Uncompressed CBOR-LD v0.6 is not supported.");
        }

        final DocumentDictionary dictionary = config.registry().get(Byte.toUnsignedInt(encoded[2]));

        if (dictionary == null) {
            throw new DecoderException(DecoderCode.UNKNOWN_DICTIONARY,
                    "Unknown CBOR-LD v0.6 document terms dictionary code = "
                            + Hex.toString(encoded[2]) + ".");
        }

        return decode(dictionary, encoded);
    }
}
