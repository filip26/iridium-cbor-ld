package com.apicatalog.cborld.encoder.value;

import java.nio.ByteBuffer;
import java.util.Collection;
import java.util.UUID;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.json.JsonUtils;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class UuidValueEncoder implements ValueEncoder {

    public static final String PREFIX = "urn:uuid:";
    public static final int CODE = 3;

    @Override
    public DataItem encode(Mapping mapping, JsonValue jsonValue, String term, Collection<String> types) {

        if (JsonUtils.isString(jsonValue) && ((JsonString) jsonValue).getString().toLowerCase().startsWith(PREFIX)) {

            String rest = ((JsonString) jsonValue).getString().substring(PREFIX.length());

            Array result = new Array();

            result.add(new UnsignedInteger(CODE));
            result.add(new ByteString(toBytes(UUID.fromString(rest))));

            return result;
        }
        return null;
    }

    public static byte[] toBytes(UUID uuid) {
        ByteBuffer bb = ByteBuffer.wrap(new byte[16]);
        bb.putLong(uuid.getMostSignificantBits());
        bb.putLong(uuid.getLeastSignificantBits());
        return bb.array();
    }
}
