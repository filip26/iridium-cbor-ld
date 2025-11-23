package com.apicatalog.cborld.decoder.value;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class VocabValueDecoder implements ValueDecoder {

    @Override
    public String decode(Mapping mapping, DataItem value, String term, String type) throws DecoderException {
        return (Keywords.VOCAB.equals(type)
                && mapping != null
                && mapping.termMap() != null
                && value instanceof UnsignedInteger uint)
                        ? mapping.termMap().getValue(uint.getValue().intValueExact())
                        : null;
    }
}
