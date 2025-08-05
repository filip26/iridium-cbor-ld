package com.apicatalog.cborld.decoder.value;

import java.time.Instant;
import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class XsdDateTimeValueDecoder implements ValueDecoder {

    public static final String DATE_TIME = "http://www.w3.org/2001/XMLSchema#dateTime";

    @Override
    public JsonValue decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderError {
        if (types != null
                && types.contains(DATE_TIME)
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())) {

            long epochSeconds = ((UnsignedInteger) value).getValue().longValueExact();

            return Json.createValue(Instant.ofEpochSecond(epochSeconds).toString());
        }
        return null;

    }
}
