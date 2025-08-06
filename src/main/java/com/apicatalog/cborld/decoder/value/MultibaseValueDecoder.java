package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.encoder.value.MultibaseValueEncoder;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.multibase.MultibaseDecoder;

import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class MultibaseValueDecoder implements ValueDecoder {

    static final MultibaseDecoder MULTIBASE = MultibaseDecoder.getInstance();

    @Override
    public JsonValue decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderException {

        if (types != null && types.contains(MultibaseValueEncoder.TYPE)
                && MajorType.BYTE_STRING.equals(value.getMajorType())) {

            byte[] byteString = ((ByteString) value).getBytes();

            if (byteString.length <= 1) {
                return null;
            }

            return MULTIBASE.getBase((char) byteString[0])
                    .map(base -> {
                        byte[] data = new byte[byteString.length - 1];
                        System.arraycopy(byteString, 1, data, 0, byteString.length - 1);

                        String encoded = base.encode(data);
                        if (encoded != null) {
                            return Json.createValue(encoded);
                        }

                        return null;

                    })
                    .orElse(null);
        }
        return null;
    }
}
