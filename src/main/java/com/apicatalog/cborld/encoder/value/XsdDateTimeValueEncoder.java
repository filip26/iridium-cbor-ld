package com.apicatalog.cborld.encoder.value;

import java.time.Instant;
import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.json.cursor.JsonValueCursor;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateTimeValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Dictionary dictionary, JsonValueCursor value, String term, Collection<String> types) {
//System.out.println("--- " + term);
//System.out.println(value);
//System.out.println(def);
        if (types != null && types.contains("http://www.w3.org/2001/XMLSchema#dateTime")
                && value.isString()
                ) {
            
            final Instant instant = Instant.parse(value.stringValue());
            
            return new UnsignedInteger(instant.getEpochSecond());
        }
        return null;
    }
}
