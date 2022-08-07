package com.apicatalog.cborld.encoder.value;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cursor.ValueCursor;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Dictionary dictionary, ValueCursor value, String term, Collection<String> types) {

        if (types != null && types.contains("http://www.w3.org/2001/XMLSchema#date")
                && value.isString()
                ) {
            
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            LocalDate date = LocalDate.parse(value.stringValue(), formatter);

            return new UnsignedInteger(date.atStartOfDay(ZoneId.of("Z")).toEpochSecond());
        }
        return null;
    }
}
