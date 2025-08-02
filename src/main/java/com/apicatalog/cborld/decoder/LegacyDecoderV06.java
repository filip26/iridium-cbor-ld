package com.apicatalog.cborld.decoder;

import java.io.ByteArrayInputStream;
import java.util.List;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.cborld.registry.DocumentDictionary;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;
import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonValue;

public class LegacyDecoderV06 extends BaseDecoder {

    public LegacyDecoderV06(DecoderConfig config) {
        super(config);
    }

    @Override
    public JsonValue decode(byte[] encoded) throws ContextError, DecoderError {
        CborLdVersion version = BaseDecoder.assertCborLd(encoded);

        if (version != CborLdVersion.V06) {
            throw new DecoderError(Code.Unsupported, "The decoder does support " + version + " but " + CborLdVersion.V06 + " .");
        }

        return decode(version, encoded);
    }

    @Override
    public JsonValue decode(CborLdVersion version, byte[] encoded) throws ContextError, DecoderError {

        if (encoded[2] == CborLd.UNCOMPRESSED_BYTE) {
            throw new DecoderError(Code.Unsupported, "Uncompressed CBOR-LD v0.6 is not supported.");            
        }
        
        final DocumentDictionary dictionary = config.registry().get(Byte.toUnsignedInt(encoded[2]));

        if (dictionary == null) {
            throw new DecoderError(Code.UnknownCompression,
                    "Unkknown CBOR-LD document terms dictionary type id, found ["
                            + Hex.toString(encoded[2]) + "].");
        }

        return decode(encoded, dictionary);
    }

    protected JsonValue decode(byte[] encoded, final DocumentDictionary provider) throws ContextError, DecoderError {
        try {
            final ByteArrayInputStream bais = new ByteArrayInputStream(encoded);
            final List<DataItem> dataItems = new CborDecoder(bais).decode();

            // nothing do de-compress
            if (dataItems.isEmpty()) {
                return null;
            }

            // only one object
            if (dataItems.size() == 1) {
                return decode(dataItems.iterator().next(), provider);
            }

            // decode as an array of objects
            final JsonArrayBuilder builder = Json.createArrayBuilder();

            for (final DataItem item : dataItems) {
                builder.add(decode(item, provider));
            }

            return builder.build();

        } catch (final CborException e) {
            throw new DecoderError(Code.InvalidDocument, e);
        }
    }

}
