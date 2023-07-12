package com.apicatalog.cborld.decoder.value;

import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.TimeZone;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.dictionary.Dictionary;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class XsdDateValueDecoder implements ValueDecoder {

    @Override
    public JsonValue decode(Dictionary dictionary, DataItem value, String term, Collection<String> types) throws DecoderError {
        if (types != null && types.contains("http://www.w3.org/2001/XMLSchema#date")
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())
                ) {

            long epochSeconds = ((UnsignedInteger)value).getValue().longValue();
            
            final Date date = new Date(epochSeconds*1000);
                        
            SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
            formatter.setTimeZone(TimeZone.getTimeZone("GMT"));
            
            return Json.createValue(formatter.format(date));            
        }
        return null;
    }
}
