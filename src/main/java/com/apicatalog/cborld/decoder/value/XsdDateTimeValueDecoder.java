package com.apicatalog.cborld.decoder.value;

import java.time.DateTimeException;
import java.time.Instant;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.decoder.DecoderException.DecoderError;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateTimeValueDecoder implements ValueDecoder {

    public static final String DATE_TIME_TYPE = "http://www.w3.org/2001/XMLSchema#dateTime";

    @Override
    public String decode(Mapping mapping, DataItem value, String term, String type) throws DecoderException {
        try {
            return (DATE_TIME_TYPE.equals(type)
                    && value instanceof UnsignedInteger epochSeconds)
                            ? Instant.ofEpochSecond(epochSeconds.getValue().longValueExact()).toString()
                            : null;
        } catch (DateTimeException e) {
            throw new DecoderException(DecoderError.INVALID_VALUE, e);
        }
    }
}
