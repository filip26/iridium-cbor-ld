package com.apicatalog.cborld.encoder.value;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.encoder.EncoderError;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.json.JsonUtils;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class CustomTypeValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Mapping mapping, JsonValue jsonValue, String term, Collection<String> types) throws EncoderError {
        if (types != null
                && mapping.dictionary() != null
                && mapping.dictionary().types() != null
                && JsonUtils.isString(jsonValue)) {

            final Map<String, Dictionary> typeMap = mapping.dictionary().types();
            final String value = ((JsonString) jsonValue).getString();

            for (final String type : types) {

                final Dictionary dictionary = typeMap.get(type);

                if (dictionary == null) {
                    continue;
                }
                final Integer code = dictionary.getCode(value);
                if (code != null) {
                    return new UnsignedInteger(code);
                }
            }
        }

        return null;
    }

}
