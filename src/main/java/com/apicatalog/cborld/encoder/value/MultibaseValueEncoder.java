package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.multibase.MultibaseDecoder;

import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;

public class MultibaseValueEncoder implements ValueEncoder {

    public static final String TYPE = "https://w3id.org/security#multibase";

    static final MultibaseDecoder MULTIBASE = MultibaseDecoder.getInstance();

    @Override
    public DataItem encode(Mapping mapping, String value, String term, Collection<String> types) {
        if (types.contains(TYPE)) {

            final String encoded = value;

            return MULTIBASE.getBase(encoded)
                    .map(base -> {
                        byte[] bytes = base.decode(encoded);
                        if (bytes != null) {

                            byte[] compressed = new byte[bytes.length + 1];

                            compressed[0] = (byte) base.prefix();

                            System.arraycopy(bytes, 0, compressed, 1, bytes.length);

                            return new ByteString(compressed);
                        }
                        return null;
                    })
                    .orElse(null);
        }
        return null;
    }
}
