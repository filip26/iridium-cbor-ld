package com.apicatalog.cborld.decoder.value;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.decoder.DecoderException.DecoderError;
import com.apicatalog.cborld.encoder.value.DidKeyValueEncoder;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.multibase.Multibase;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class DidKeyValueDecoder implements ValueDecoder {

    @Override
    public String decode(Mapping mapping, DataItem value, String term, String type) throws DecoderException {

        if (value instanceof Array array) {

            var items = array.getDataItems();

            if (items.size() >= 2
                    && items.size() <= 3
                    && items.get(0) instanceof UnsignedInteger code
                    && DidKeyValueEncoder.CODE == code.getValue().longValueExact()) {

                if (items.get(1) instanceof ByteString part1) {

                    var key = decode(part1);

                    if (items.size() == 2) {
                        return DidKeyValueEncoder.PREFIX + key;
                    }

                    if (items.get(2) instanceof ByteString part2) {
                        return DidKeyValueEncoder.PREFIX + key + "#" + decode(part2);
                    }
                }

                throw new DecoderException(DecoderError.INVALID_VALUE, "Invalid did:key value=" + value);
            }
        }
        return null;
    }

    static final String decode(ByteString dataItem) {
        return Multibase.BASE_58_BTC.encode(dataItem.getBytes());
    }
}
