package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class ContextValueDecoder implements ValueDecoder {

    @Override
    public String decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderException {
        return (Keywords.CONTEXT.equals(term)
                && mapping != null
                && mapping.dictionary() != null
                && mapping.dictionary().contexts() != null
                && value instanceof UnsignedInteger integer)
                        ? mapping.dictionary().contexts().getValue(integer.getValue().intValueExact())
                        : null;
    }
}
