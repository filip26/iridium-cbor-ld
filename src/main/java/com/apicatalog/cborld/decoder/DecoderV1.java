package com.apicatalog.cborld.decoder;

import java.io.ByteArrayInputStream;
import java.util.Iterator;
import java.util.List;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.JsonValue;

public class DecoderV1 extends BaseDecoder {

    public DecoderV1(DecoderConfig config, DocumentLoader loader) {
        super(config, loader);
    }

    @Override
    public JsonValue decode(CborLdVersion version, byte[] encoded) throws ContextError, DecoderError {
        try {
            final ByteArrayInputStream bais = new ByteArrayInputStream(encoded);
            final List<DataItem> dataItems = new CborDecoder(bais).decode();

            if (dataItems.size() != 2) {
                throw new DecoderError(Code.InvalidDocument, "The document is not CBOR-LD v1.0 document. Must start with array of two items but size = " + dataItems.size() + ".");
            }

            final Iterator<DataItem> it = dataItems.iterator();

            final DataItem registryId = it.next();

            if (registryId == null || registryId.getMajorType() != MajorType.UNSIGNED_INTEGER) {
                throw new DecoderError(Code.InvalidDocument, "The document is not CBOR-LD v1.0 document. Registry Entry ID is not an unsigned integer but " + registryId + ".");
            }

            final DocumentDictionary dictionary = config.registry().get(
                    ((UnsignedInteger) registryId).getValue().intValueExact());

            return decode(it.next(), dictionary);

        } catch (final CborException e) {
            throw new DecoderError(Code.InvalidDocument, e);
        }
    }
}
