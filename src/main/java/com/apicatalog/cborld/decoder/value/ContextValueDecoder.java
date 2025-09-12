package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;

public class ContextValueDecoder implements ValueDecoder {

    @Override
    public String decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderException {
        if (mapping != null
                && mapping.dictionary() != null
                && mapping.dictionary().contexts() != null
                && Keywords.CONTEXT.equals(term)
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())) {
            return mapping.dictionary().contexts().getValue(((UnsignedInteger) value).getValue().intValueExact());
        }
        return null;
    }
}
