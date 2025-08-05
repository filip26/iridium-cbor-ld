package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class IdValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Mapping mapping, JsonValue jsonValue, String term, Collection<String> types) {
        if (types != null && types.contains(Keywords.ID)) {

            final String id = ((JsonString) jsonValue).getString();

            Integer code = mapping.uris().getCode(id);

            if (code == null) {
                code = mapping.terms().getCode(id);
            }

            if (code != null) {
                return new UnsignedInteger(code);
            }
        }
        return null;
    }
}
