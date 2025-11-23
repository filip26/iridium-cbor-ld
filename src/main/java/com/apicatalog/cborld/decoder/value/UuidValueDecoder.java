package com.apicatalog.cborld.decoder.value;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.UUID;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.encoder.value.UuidValueEncoder;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class UuidValueDecoder implements ValueDecoder {

    @Override
    public String decode(Mapping mapping, DataItem value, String term, String types) throws DecoderException {
        return (value instanceof Array array && array.getDataItems().size() == 2
                && array.getDataItems().get(0) instanceof UnsignedInteger code
                && code.getValue().equals(BigInteger.valueOf(UuidValueEncoder.CODE))
                && array.getDataItems().get(1) instanceof ByteString uuid)
                        ? UuidValueEncoder.PREFIX + of(uuid.getBytes()).toString()
                        : null;
    }

    public static UUID of(byte[] bytes) {
        final var byteBuffer = ByteBuffer.wrap(bytes);

        return new UUID(
                byteBuffer.getLong(),
                byteBuffer.getLong());
    }
}
