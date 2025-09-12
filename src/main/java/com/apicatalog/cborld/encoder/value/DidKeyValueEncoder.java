package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.multibase.Multibase;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class DidKeyValueEncoder implements ValueEncoder {

    public static final String PREFIX = "did:key:";
    public static final int CODE = 1025;

    @Override
    public DataItem encode(Mapping mapping, String value, String term, Collection<String> types) {

        if (value == null
                || !(value.toLowerCase().startsWith(PREFIX))) {
            return null;
        }

        String encoded = value.substring(PREFIX.length());

        String fragment = null;

        final int fragmentIndex = encoded.indexOf('#');
        if (fragmentIndex != -1) {
            fragment = encoded.substring(fragmentIndex + 1);
            encoded = encoded.substring(0, fragmentIndex);
        }

        final Array result = new Array();

        result.add(new UnsignedInteger(CODE));
        result.add(encode(encoded));

        if (fragment != null) {
            result.add(encode(fragment));
        }

        return result;
    }

    static final ByteString encode(String value) {
        return new ByteString(Multibase.BASE_58_BTC.decode(value));
    }
}
