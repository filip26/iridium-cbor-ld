package com.apicatalog.cborld.decoder.value;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.Collection;
import java.util.UUID;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.encoder.value.UuidValueEncoder;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class UuidValueDecoder implements ValueDecoder {

    @Override
    public JsonValue decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderError {

        if (MajorType.ARRAY.equals(value.getMajorType())) {

            final Array data = (Array) value;

            if (data.getDataItems().size() == 2) {

                final DataItem code = data.getDataItems().get(0);
                final DataItem uuid = data.getDataItems().get(1);

                if (MajorType.UNSIGNED_INTEGER.equals(code.getMajorType())
                        && ((UnsignedInteger) code).getValue().equals(BigInteger.valueOf(3))
                        && MajorType.BYTE_STRING.equals(uuid.getMajorType())) {
                    return Json.createValue(UuidValueEncoder.PREFIX + of(((ByteString) uuid).getBytes()).toString());
                }
            }
        }
        return null;
    }

    public static UUID of(byte[] bytes) {
        ByteBuffer byteBuffer = ByteBuffer.wrap(bytes);
        long high = byteBuffer.getLong();
        long low = byteBuffer.getLong();
        return new UUID(high, low);
    }
}
