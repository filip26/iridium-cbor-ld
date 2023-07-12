package com.apicatalog.cborld.encoder.value;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.TimeZone;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cursor.ValueCursor;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateTimeValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Dictionary dictionary, ValueCursor value, String term, Collection<String> types) {

        if (types != null && types.contains("http://www.w3.org/2001/XMLSchema#dateTime")
                && value.isString()
                ) {
            
            DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
            formatter.setTimeZone(TimeZone.getTimeZone("GMT"));
            
            try {
                Date date = formatter.parse(value.stringValue());

                return new UnsignedInteger(date.getTime() / 1000);
                
            } catch (ParseException e) {
                
            }
            
//            final Instant instant = Instant.parse(value.stringValue());
            

        }
        return null;
    }
}
