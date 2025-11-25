package com.apicatalog.cborld.encoder.value;

import java.nio.ByteBuffer;
import java.util.UUID;

import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class UuidValueEncoder implements ValueEncoder {

    public static final String PREFIX = "urn:uuid:";
    public static final int CODE = 3;

    @Override
    public DataItem encode(Mapping mapping, String value, String term, String type) {

        if (value != null && value.toLowerCase().startsWith(PREFIX)) {

            final var rest = value.substring(PREFIX.length());

            final var result = new Array();

            result.add(new UnsignedInteger(CODE));
            result.add(new ByteString(toBytes(UUID.fromString(rest))));

            return result;
        }
        return null;
    }

    public static byte[] toBytes(final UUID uuid) {
        return ByteBuffer.wrap(new byte[16])
                .putLong(uuid.getMostSignificantBits())
                .putLong(uuid.getLeastSignificantBits())
                .array();
    }
}
