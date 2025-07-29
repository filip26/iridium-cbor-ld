package com.apicatalog.cborld.encoder.value;

import java.util.Collection;
import java.util.UUID;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.uuid.Uuid;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class UuidValueEncoder implements ValueEncoder {

    public final static String PREFIX = "urn:uuid:";
    public final static int CODE = 3;

    @Override
    public DataItem encode(Mapping mapping, JsonValue jsonValue, String term, Collection<String> types) {

        if (JsonUtils.isString(jsonValue) && ((JsonString) jsonValue).getString().toLowerCase().startsWith(PREFIX)) {

            String rest = ((JsonString) jsonValue).getString().substring(PREFIX.length());

            Array result = new Array();

            result.add(new UnsignedInteger(CODE));
            result.add(new ByteString(Uuid.toBytes(UUID.fromString(rest))));

            return result;
        }
        return null;
    }
}
