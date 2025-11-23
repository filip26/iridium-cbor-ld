package com.apicatalog.cborld.encoder.value;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateValueEncoder implements ValueEncoder {

    public static final String DATE = "http://www.w3.org/2001/XMLSchema#date";

    @Override
    public DataItem encode(Mapping mapping, String value, String term, String type) {
        if (DATE.equals(type)) {

            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            LocalDate date = LocalDate.parse(value, formatter);

            return new UnsignedInteger(date.toEpochSecond(LocalTime.MIDNIGHT, ZoneOffset.UTC));
        }
        return null;
    }
}
