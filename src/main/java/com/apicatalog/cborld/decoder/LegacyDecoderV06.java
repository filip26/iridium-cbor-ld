package com.apicatalog.cborld.decoder;

import java.net.URI;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

import jakarta.json.JsonValue;

public class LegacyDecoderV06 extends BaseDecoder {

    public LegacyDecoderV06(DecoderConfig config, DocumentLoader loader, URI base) {
        super(config, loader, base);
    }

    @Override
    public JsonValue decode(CborLdVersion version, byte[] encoded) throws ContextError, DecoderError {

        if (encoded[2] == CborLd.UNCOMPRESSED_BYTE) {
            throw new DecoderError(Code.Unsupported, "Uncompressed CBOR-LD v0.6 is not supported.");
        }

        final DocumentDictionary dictionary = config.registry().get(Byte.toUnsignedInt(encoded[2]));

        if (dictionary == null) {
            throw new DecoderError(Code.UnknownCompression,
                    "Unkknown CBOR-LD document terms dictionary type id, found "
                            + Hex.toString(encoded[2]) + ".");
        }

        return decode(encoded, dictionary);
    }
}
