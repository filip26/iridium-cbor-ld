package com.apicatalog.cborld.encoder.value;

import java.math.BigInteger;
import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cursor.ValueCursor;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class StringValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Dictionary dictionary, ValueCursor value, String term, Collection<String> types) {

        if (value.isString()) {
            
            BigInteger code = dictionary.getCode(value.stringValue());
            
            if (code != null) {
                return new UnsignedInteger(code);
            }
        }
        return null;
    }
}
