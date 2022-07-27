package com.apicatalog.cborld.encoder.value;

import java.math.BigInteger;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.json.cursor.JsonValueCursor;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class ContextValueEncoder implements ValueEncoder {

    protected final Dictionary contexts;

    public ContextValueEncoder(final Dictionary dictionary) {
        this.contexts = dictionary;
    }

    @Override
    public DataItem encode(final Dictionary dictionary, final JsonValueCursor value, final String term, final TermDefinition def) {
        if (Keywords.CONTEXT.equals(term)) {
    
            final BigInteger code = contexts.getCode(value.stringValue());
    
            if (code != null) {
                return new UnsignedInteger(code);
            }
        }
        return null;
    }
}
