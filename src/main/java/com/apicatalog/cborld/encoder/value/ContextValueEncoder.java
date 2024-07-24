package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class ContextValueEncoder implements ValueEncoder {

    protected final Dictionary contexts;

    public ContextValueEncoder(final Dictionary dictionary) {
        this.contexts = dictionary;
    }

    @Override
    public DataItem encode(final Dictionary dictionary, final ValueCursor value, final String term, Collection<String> types) {
        if (Keywords.CONTEXT.equals(term)) {
    
            final Integer code = contexts.getCode(value.stringValue());
    
            if (code != null) {
                return new UnsignedInteger(code);
            }
        }
        return null;
    }
}
