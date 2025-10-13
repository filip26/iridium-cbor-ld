package com.apicatalog.cborld.decoder.value;

import java.util.Collection;
import java.util.List;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.encoder.value.DidKeyValueEncoder;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.multibase.Multibase;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class DidKeyValueDecoder implements ValueDecoder {

    @Override
    public String decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderException {

        if (value instanceof Array array) {

            final List<DataItem> items = array.getDataItems();

            if (items.size() < 2 || items.size() > 3) {
                return null;
            }

            final DataItem code = items.get(0);
            final DataItem part1 = items.get(1);
            final DataItem part2 = items.size() == 3 ? items.get(2) : null;

            if (code instanceof UnsignedInteger uint
                    && DidKeyValueEncoder.CODE == uint.getValue().longValueExact()
                    && part1 instanceof ByteString stringKey
                    && (part2 == null || part2 instanceof ByteString)) {

                var key = decode(stringKey);

                var fragment = part2 != null
                        ? "#" + decode((ByteString) part2)
                        : "";

                return DidKeyValueEncoder.PREFIX + key + fragment;
            }
        }
        return null;
    }

    static final String decode(ByteString dataItem) {
        return Multibase.BASE_58_BTC.encode(dataItem.getBytes());
    }
}
