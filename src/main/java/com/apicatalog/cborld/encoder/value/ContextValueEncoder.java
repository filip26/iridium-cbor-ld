package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class ContextValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(final Mapping mapping, final String value, final String term, Collection<String> types) {
        if (Keywords.CONTEXT.equals(term)
                && mapping != null
                && mapping.dictionary() != null
                && mapping.dictionary().contexts() != null) {

            final Integer code = mapping.dictionary().contexts().getCode(value);

            if (code != null) {
                return new UnsignedInteger(code);
            }
        }
        return null;
    }
}
