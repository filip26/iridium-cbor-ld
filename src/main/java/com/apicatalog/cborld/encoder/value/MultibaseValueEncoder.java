package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.multibase.MultibaseDecoder;

import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;

public class MultibaseValueEncoder implements ValueEncoder {

    public static final String TYPE = "https://w3id.org/security#multibase";

    static final MultibaseDecoder MULTIBASE = MultibaseDecoder.getInstance();

    @Override
    public DataItem encode(Dictionary dictionary, ValueCursor value, String term, Collection<String> types) {
        if (value.isString() && types != null && types.contains(TYPE)) {

            final String based = value.stringValue();

            return MULTIBASE.getBase(based)
                    .map(base -> {
                        byte[] bytes = base.decode(value.stringValue());
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
