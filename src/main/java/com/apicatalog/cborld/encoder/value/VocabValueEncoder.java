package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class VocabValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Mapping mapping, ValueCursor value, String term, Collection<String> types) {

        if (value.isString() && types != null && types.contains(Keywords.VOCAB)) {

            Integer code = mapping.terms().getCode(value.stringValue());

            if (code != null) {
                return new UnsignedInteger(code);
            }
        }
        return null;
    }
}
