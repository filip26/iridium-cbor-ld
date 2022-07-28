package com.apicatalog.cborld.encoder.value;

import java.time.Instant;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.json.cursor.JsonValueCursor;
import com.apicatalog.jsonld.context.TermDefinition;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class XsdDateTimeValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Dictionary dictionary, JsonValueCursor value, String term, TermDefinition def) {

        if (def != null 
                && "http://www.w3.org/2001/XMLSchema#dateTime".equals(def.getTypeMapping())
                && value.isString()
                ) {
            
            final Instant instant = Instant.parse(value.stringValue());
            
            return new UnsignedInteger(instant.getEpochSecond());
        }
        return null;
    }
}
