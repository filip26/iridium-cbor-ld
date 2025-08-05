package com.apicatalog.cborld.encoder.value;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.json.JsonUtils;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class XsdDateValueEncoder implements ValueEncoder {

    public static final String DATE = "http://www.w3.org/2001/XMLSchema#date";

    @Override
    public DataItem encode(Mapping mapping, JsonValue jsonValue, String term, Collection<String> types) {

        if (types != null && types.contains(DATE) && JsonUtils.isString(jsonValue)) {

            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            LocalDate date = LocalDate.parse(((JsonString) jsonValue).getString(), formatter);

            return new UnsignedInteger(date.toEpochSecond(LocalTime.MIDNIGHT, ZoneOffset.UTC));
        }
        return null;
    }
}
