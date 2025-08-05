package com.apicatalog.cborld.encoder.value;

import java.time.Instant;
import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.json.JsonUtils;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class XsdDateTimeValueEncoder implements ValueEncoder {

    public static final String DATE_TIME = "http://www.w3.org/2001/XMLSchema#dateTime";

    @Override
    public DataItem encode(Mapping mapping, JsonValue jsonValue, String term, Collection<String> types) {

        if (types != null && types.contains(DATE_TIME) && JsonUtils.isString(jsonValue)) {

            final Instant instant = Instant.parse(((JsonString) jsonValue).getString());

            return new UnsignedInteger(instant.getEpochSecond());
        }
        return null;
    }
}
