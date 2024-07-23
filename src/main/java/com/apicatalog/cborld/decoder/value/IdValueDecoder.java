package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class IdValueDecoder implements ValueDecoder {

    @Override
    public JsonValue decode(Dictionary dictionary, DataItem value, String term, Collection<String> types) throws DecoderError {
        if (types != null && types.contains(Keywords.ID)
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())
                ) {
            final String type= dictionary.getValue(((UnsignedInteger)value).getValue());
    
            if (type != null) {
                return Json.createValue(type);
            }
        }
        return null;
    }

}
