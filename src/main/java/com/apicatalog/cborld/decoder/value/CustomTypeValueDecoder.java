package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.dictionary.Dictionaries;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import jakarta.json.JsonValue;

public class CustomTypeValueDecoder implements ValueDecoder {

    @Override
    public JsonValue decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderError {

        if (mapping != null
                && types != null
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())) {
System.out.println(types);
//            types.stream().map(dictionaries::get).findFirst()
//                    .map(dictionary -> {
//                        if (dictionary != null) {
//                            final String type = dictionary.getValue(((UnsignedInteger) value).getValue());
//
//                            if (type != null) {
//                                return Json.createValue(type);
//                            }
//                        }
//                        return null;
//                    }).orElse(null);
        }
        return null;
    }
}
