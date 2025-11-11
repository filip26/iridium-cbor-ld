package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class CustomTypeValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Mapping mapping, String value, String term, Collection<String> types) throws EncoderException {
        if (!types.isEmpty()
                && mapping != null
                && mapping.dictionary() != null
                && mapping.dictionary().types() != null) {

            var typeMap = mapping.dictionary().types();

            for (final String type : types) {

                final Dictionary dictionary = typeMap.get(type);

                if (dictionary == null) {
                    continue;
                }

                var code = dictionary.getCode(value);

                if (code != null) {
                    return new UnsignedInteger(code);
                }
            }
        }
        return null;
    }
}
