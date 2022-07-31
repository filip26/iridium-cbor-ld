package com.apicatalog.cborld.encoder.value;

import java.time.Instant;
import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cursor.ValueCursor;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateTimeValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Dictionary dictionary, ValueCursor value, String term, Collection<String> types) {

        if (types != null && types.contains("http://www.w3.org/2001/XMLSchema#dateTime")
                && value.isString()
                ) {
            
            final Instant instant = Instant.parse(value.stringValue());
            
            return new UnsignedInteger(instant.getEpochSecond());
        }
        return null;
    }
}
