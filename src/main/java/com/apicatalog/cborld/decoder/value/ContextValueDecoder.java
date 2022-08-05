package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class ContextValueDecoder implements ValueDecoder {

    protected final Dictionary contexts;

    public ContextValueDecoder(final Dictionary dictionary) {
        this.contexts = dictionary;
    }

    @Override
    public JsonValue decode(Dictionary dictionary, DataItem value, String term, Collection<String> types) throws DecoderError {

        if (Keywords.CONTEXT.equals(term)
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())
                ) {
            
            final String decoded = contexts.getValue(((UnsignedInteger)value).getValue());
    
            if (decoded != null) {
                return Json.createValue(decoded);
            }
        }
        return null;
    }
}
