package com.apicatalog.cborld.decoder;

import java.net.URI;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderException.Code;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

class LegacyDecoderV06 extends AbstractDecoder {

    public LegacyDecoderV06(DecoderConfig config, DecoderMappingProvider mapping, DocumentLoader loader, URI base) {
        super(config, mapping, loader, base);
    }

    @Override
    public Object decode(CborLdVersion version, byte[] encoded) throws ContextError, DecoderException {

        if (encoded[2] == UNCOMPRESSED_BYTE) {
            throw new DecoderException(Code.Unsupported, "Uncompressed CBOR-LD v0.6 is not supported.");
        }

        final DocumentDictionary dictionary = config.registry().get(Byte.toUnsignedInt(encoded[2]));

        if (dictionary == null) {
            throw new DecoderException(Code.UnknownDictionary,
                    "Unknown CBOR-LD v0.6 document terms dictionary code = "
                            + Hex.toString(encoded[2]) + ".");
        }

        return decode(dictionary, encoded);
    }
}
