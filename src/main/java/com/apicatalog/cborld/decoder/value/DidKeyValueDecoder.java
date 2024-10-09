package com.apicatalog.cborld.decoder.value;

import java.util.Collection;
import java.util.List;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.encoder.value.DidKeyValueEncoder;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.multibase.Multibase;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class DidKeyValueDecoder implements ValueDecoder {

    @Override
    public JsonValue decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderError {

        if (MajorType.ARRAY.equals(value.getMajorType())) {

            final List<DataItem> items = ((Array) value).getDataItems();

            if (items.size() < 2 || items.size() > 3) {
                return null;
            }

            final DataItem code = items.get(0);
            final DataItem part1 = items.get(1);
            final DataItem part2 = items.size() == 3 ? items.get(2) : null;

            if (!MajorType.UNSIGNED_INTEGER.equals(code.getMajorType())
                    || DidKeyValueEncoder.CODE != ((UnsignedInteger) code).getValue().longValueExact()
                    || !MajorType.BYTE_STRING.equals(part1.getMajorType())
                    || (part2 != null && !MajorType.BYTE_STRING.equals(part2.getMajorType()))) {
                return null;
            }

            final String key = decode((ByteString) part1);

            final String fragment = part2 != null
                    ? "#" + decode((ByteString) part2)
                    : "";

            return Json.createValue(DidKeyValueEncoder.PREFIX + key + fragment);
        }

        return null;
    }

    static final String decode(ByteString dataItem) {
        return Multibase.BASE_58_BTC.encode(dataItem.getBytes());
    }
}
