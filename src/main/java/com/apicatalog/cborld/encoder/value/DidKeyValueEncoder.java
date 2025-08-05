package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.multibase.Multibase;
import com.apicatalog.multicodec.Multicodec.Tag;
import com.apicatalog.multicodec.MulticodecDecoder;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class DidKeyValueEncoder implements ValueEncoder {

    public final static String PREFIX = "did:key:";
    public final static int CODE = 1025;

    protected static MulticodecDecoder CODECS = MulticodecDecoder.getInstance(Tag.Key);

    @Override
    public DataItem encode(Mapping mapping, JsonValue jsonValue, String term, Collection<String> types) {

        if (JsonUtils.isNotString(jsonValue)
                || !((JsonString) jsonValue).getString().toLowerCase().startsWith(PREFIX)) {
            return null;
        }

        String encoded = ((JsonString) jsonValue).getString().substring(PREFIX.length());

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
