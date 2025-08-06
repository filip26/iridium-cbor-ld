package com.apicatalog.cborld.decoder;

import java.io.ByteArrayInputStream;
import java.net.URI;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderException.Code;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

class DecoderV1 extends BaseDecoder {

    public DecoderV1(DecoderConfig config, DecoderMappingProvider mapping, DocumentLoader loader, URI base) {
        super(config, mapping, loader, base);
    }

    @Override
    public JsonValue decode(CborLdVersion version, byte[] encoded) throws ContextError, DecoderException {
        try {
            var bais = new ByteArrayInputStream(encoded);
            var dataItems = new CborDecoder(bais).decode();

            // nothing do de-compress
            if (dataItems.isEmpty()) {
                return null;
            }
            
            if (dataItems.size() == 1) {
                return decode(version, dataItems.iterator().next());
            }

            var arrayBuilder = Json.createArrayBuilder();

            for (var item : dataItems) {
                arrayBuilder.add(decode(version, item));
            }

            return arrayBuilder.build();

        } catch (final CborException e) {
            throw new DecoderException(Code.InvalidDocument, e);
        }
    }

    public JsonValue decode(CborLdVersion version, DataItem dataItem) throws ContextError, DecoderException {
        if (dataItem == null || dataItem.getMajorType() != MajorType.ARRAY) {
            throw new DecoderException(Code.InvalidDocument, "The document is not CBOR-LD v1.0 document. Must start with array of two items, but is " + dataItem + ".");
        }

        if (((Array) dataItem).getDataItems().size() != 2) {
            throw new DecoderException(Code.InvalidDocument, "The document is not CBOR-LD v1.0 document. Must start with array of two items but is " + dataItem + ".");
        }

        var it = ((Array) dataItem).getDataItems().iterator();

        var registryId = it.next();

        if (registryId == null || registryId.getMajorType() != MajorType.UNSIGNED_INTEGER) {
            throw new DecoderException(Code.InvalidDocument, "The document is not CBOR-LD v1.0 document. Registry Entry ID is not an unsigned integer but " + registryId + ".");
        }

        var dictionary = config.registry().get(
                ((UnsignedInteger) registryId).getValue().intValueExact());

        if (dictionary == null) {
            throw new DecoderException(Code.UnknownDictionary,
                    "Unknown CBOR-LD document terms dictionary code = "
                            + registryId
                            + ", hex = "
                            + Hex.toString(((UnsignedInteger) registryId).getValue().intValueExact()) + ".");
        }

        return decode(dictionary, it.next());
    }

}
