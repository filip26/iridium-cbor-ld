package com.apicatalog.cborld.encoder.value;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.cborld.encoder.EncoderException.EncoderError;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateValueEncoder implements ValueEncoder {

    public static final String DATE = "http://www.w3.org/2001/XMLSchema#date";

    @Override
    public DataItem encode(Mapping mapping, String value, String term, String type) throws EncoderException {
        if (DATE.equals(type)) {
            try {
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
                LocalDate date = LocalDate.parse(value, formatter);

                return new UnsignedInteger(date.toEpochSecond(LocalTime.MIDNIGHT, ZoneOffset.UTC));

            } catch (DateTimeParseException e) {
                throw new EncoderException(EncoderError.INVALID_VALUE, e);
            }
        }
        return null;
    }
}
