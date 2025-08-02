package com.apicatalog.cborld.decoder;

import java.io.ByteArrayInputStream;
import java.util.List;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.cborld.registry.LegacyDictionary;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;
import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonValue;

public class LegacyDecoderV05 extends BaseDecoder {

    public LegacyDecoderV05(DecoderConfig config) {
        super(config);
    }

    @Override
    public JsonValue decode(byte[] encoded) throws ContextError, DecoderError {
        CborLdVersion version = BaseDecoder.assertCborLd(encoded);

        if (version != CborLdVersion.V05_COMPRESSED) {
            throw new DecoderError(Code.Unsupported, "The decoder does support " + version + " but " + CborLdVersion.V05_COMPRESSED + " .");
        }

        return decode(version, encoded);
    }

    @Override
    public JsonValue decode(CborLdVersion version, byte[] encoded) throws ContextError, DecoderError {
        return decode(encoded, LegacyDictionary.DICTIONARY);

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
