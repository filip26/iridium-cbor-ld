package com.apicatalog.cborld.encoder.value;

import java.time.Instant;
import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cursor.ValueCursor;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateTimeValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Mapping mapping, ValueCursor value, String term, Collection<String> types) {

        if (types != null && types.contains("http://www.w3.org/2001/XMLSchema#dateTime")
                && value.isString()
                ) {
            
            final Instant instant = Instant.parse(value.stringValue());
            
            return new UnsignedInteger(instant.getEpochSecond());
        }
        return null;
    }
}
