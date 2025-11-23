package com.apicatalog.cborld.decoder.value;

import java.time.DateTimeException;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.decoder.DecoderException.DecoderError;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateValueDecoder implements ValueDecoder {

    public static final String DATE_TYPE = "http://www.w3.org/2001/XMLSchema#date";

    @Override
    public String decode(Mapping mapping, DataItem value, String term, String type) throws DecoderException {
        try {
            return (DATE_TYPE.equals(type)
                    && value instanceof UnsignedInteger epochSeconds)
                            ? DateTimeFormatter
                                    .ofPattern("yyyy-MM-dd")
                                    .format(LocalDate.ofInstant(
                                            Instant.ofEpochSecond(epochSeconds.getValue().longValueExact()),
                                            ZoneOffset.UTC))
                            : null;
        } catch (DateTimeException e) {
            throw new DecoderException(DecoderError.INVALID_VALUE, e);
        }
    }
}
