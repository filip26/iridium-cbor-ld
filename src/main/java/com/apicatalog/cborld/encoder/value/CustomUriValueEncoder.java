package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.encoder.EncoderError;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.jsonld.uri.UriValidationPolicy;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class CustomUriValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Mapping mapping, JsonValue jsonValue, String term, Collection<String> types) throws EncoderError {
        if (JsonUtils.isString(jsonValue)) {
            final String value = ((JsonString) jsonValue).getString();
            if (UriUtils.isAbsoluteUri(value, UriValidationPolicy.SchemeOnly)) {

                final Dictionary dictionary = mapping.uris();

                if (dictionary != null) {
                    final Integer code = dictionary.getCode(value);
                    if (code != null) {
                        return new UnsignedInteger(code);
                    }
                }
            }
        }
        return null;
    }

}
