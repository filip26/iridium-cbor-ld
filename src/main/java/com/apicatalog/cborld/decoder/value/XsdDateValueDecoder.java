package com.apicatalog.cborld.decoder.value;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.linkedtree.xsd.XsdVocab;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class XsdDateValueDecoder implements ValueDecoder {

    @Override
    public JsonValue decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderError {
        if (types != null
                && types.contains(XsdVocab.DATE)
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())) {

            long epochSeconds = ((UnsignedInteger) value).getValue().longValueExact();

            final Instant date = Instant.ofEpochSecond(epochSeconds);

            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

            return Json.createValue(formatter.format(LocalDate.ofInstant(date, ZoneOffset.UTC)));
        }
        return null;
    }
}
