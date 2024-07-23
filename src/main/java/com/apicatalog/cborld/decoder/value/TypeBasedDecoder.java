package com.apicatalog.cborld.decoder.value;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.dictionary.Dictionary;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class TypeBasedDecoder implements ValueDecoder {

    protected final Map<String, Dictionary> dictionaries;

    public TypeBasedDecoder(Map<String, Dictionary> dictionaries) {
        this.dictionaries = dictionaries;
    }

    @Override
    public JsonValue decode(Dictionary context, DataItem value, String term, Collection<String> types) throws DecoderError {

        if (types != null
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())) {

            types.stream().map(dictionaries::get).findFirst()
                    .map(dictionary -> {
                        if (dictionary != null) {
                            final String type = dictionary.getValue(((UnsignedInteger) value).getValue());

                            if (type != null) {
                                return Json.createValue(type);
                            }
                        }
                        return null;
                    }).orElse(null);
        }
        return null;
    }
}
