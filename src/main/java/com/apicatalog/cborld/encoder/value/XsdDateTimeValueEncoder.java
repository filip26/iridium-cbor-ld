package com.apicatalog.cborld.encoder.value;

import java.time.Instant;
import java.time.format.DateTimeParseException;

import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.cborld.encoder.EncoderException.EncoderCode;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateTimeValueEncoder implements ValueEncoder {

    public static final String DATE_TIME = "http://www.w3.org/2001/XMLSchema#dateTime";

    @Override
    public DataItem encode(Mapping mapping, String value, String term, String type) throws EncoderException {
        if (DATE_TIME.equals(type)) {
            try {
                final Instant instant = Instant.parse(value);

                return new UnsignedInteger(instant.getEpochSecond());

            } catch (DateTimeParseException e) {
                throw new EncoderException(EncoderCode.INVALID_VALUE, e);
            }
        }
        return null;
    }
}
