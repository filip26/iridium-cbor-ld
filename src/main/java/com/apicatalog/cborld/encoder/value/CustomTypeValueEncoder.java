package com.apicatalog.cborld.encoder.value;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class CustomTypeValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Mapping mapping, String value, String term, String type) throws EncoderException {
        if (type != null
                && mapping != null
                && mapping.dictionary() != null
                && mapping.dictionary().types() != null) {

            var typeMap = mapping.dictionary().types();

            final Dictionary dictionary = typeMap.get(type);

            if (dictionary == null) {
                return null;
            }

            var code = dictionary.getCode(value);

            if (code != null) {
                return new UnsignedInteger(code);
            }
        }
        return null;
    }
}
