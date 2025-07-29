package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class ContextValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(final Mapping mapping, final JsonValue jsonValue, final String term, Collection<String> types) {
        if (Keywords.CONTEXT.equals(term) && JsonUtils.isString(jsonValue)) {

            final Integer code = mapping.context().getCode(((JsonString) jsonValue).getString());

            if (code != null) {
                return new UnsignedInteger(code);
            }
        }
        return null;
    }
}
