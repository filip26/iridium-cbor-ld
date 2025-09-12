package com.apicatalog.cborld.encoder.value;

import java.time.Instant;
import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateTimeValueEncoder implements ValueEncoder {

    public static final String DATE_TIME = "http://www.w3.org/2001/XMLSchema#dateTime";

    @Override
    public DataItem encode(Mapping mapping, String value, String term, Collection<String> types) {

        if (types != null && types.contains(DATE_TIME) && value != null) {

            final Instant instant = Instant.parse(value);

            return new UnsignedInteger(instant.getEpochSecond());
        }
        return null;
    }
}
