package com.apicatalog.cborld.decoder.value;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cursor.ValueCursor;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class XsdDateValueDecoder implements ValueDecoder {

    @Override
    public JsonValue decode(Dictionary dictionary, DataItem value, String term, Collection<String> types) throws DecoderError {
System.out.println("!!! " + types);
        if (types != null && types.contains("http://www.w3.org/2001/XMLSchema#date")
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())
                ) {
            
            long epochSeconds = ((UnsignedInteger)value).getValue().longValueExact();
            
            final Instant date = Instant.ofEpochSecond(epochSeconds);
            
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            
            return Json.createValue(formatter.format(date));
            
//            LocalDate date = LocalDate.parse(value.stringValue(), formatter);
                        
            //return new UnsignedInteger(date.toEpochSecond(LocalTime.MIDNIGHT, ZoneOffset.UTC));
            
  //          return Json.createValue(Instant.ofEpochSecond(epochSeconds).toString());

        }
        return null;
    }
}
