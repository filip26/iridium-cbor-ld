package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class VocabValueDecoder implements ValueDecoder {

    @Override
    public String decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderException {
        return (types.contains(Keywords.VOCAB)
                && mapping != null
                && mapping.termMap() != null
                && value instanceof UnsignedInteger uint)
                        ? mapping.termMap().getValue(uint.getValue().intValueExact())
                        : null;
    }
}
