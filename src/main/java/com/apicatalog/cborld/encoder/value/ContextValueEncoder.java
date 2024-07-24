package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class ContextValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(final Mapping mapping, final ValueCursor value, final String term, Collection<String> types) {
        if (Keywords.CONTEXT.equals(term)) {

            final Integer code = mapping.context().getCode(value.stringValue());

            if (code != null) {
                return new UnsignedInteger(code);
            }
        }
        return null;
    }
}
