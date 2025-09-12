package com.apicatalog.cborld.decoder.value;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateValueDecoder implements ValueDecoder {

    public static final String DATE = "http://www.w3.org/2001/XMLSchema#date";

    @Override
    public String decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderException {
        if (types != null
                && types.contains(DATE)
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())) {

            long epochSeconds = ((UnsignedInteger) value).getValue().longValueExact();

            final Instant date = Instant.ofEpochSecond(epochSeconds);

            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

            return formatter.format(LocalDate.ofInstant(date, ZoneOffset.UTC));
        }
        return null;
    }
}
