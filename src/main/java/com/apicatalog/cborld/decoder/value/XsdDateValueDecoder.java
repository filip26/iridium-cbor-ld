package com.apicatalog.cborld.decoder.value;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateValueDecoder implements ValueDecoder {

    public static final String DATE_TYPE = "http://www.w3.org/2001/XMLSchema#date";

    @Override
    public String decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderException {
        return (types.contains(DATE_TYPE)
                && value instanceof UnsignedInteger epochSeconds)
                        ? DateTimeFormatter
                                .ofPattern("yyyy-MM-dd")
                                .format(LocalDate.ofInstant(
                                        Instant.ofEpochSecond(epochSeconds.getValue().longValueExact()),
                                        ZoneOffset.UTC))
                        : null;
    }
}
