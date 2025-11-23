package com.apicatalog.cborld.decoder.value;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class CustomTypeValueDecoder implements ValueDecoder {

    @Override
    public String decode(final Mapping mapping, DataItem value, String term, String type) throws DecoderException {
        if (type != null
                && mapping != null
                && mapping.dictionary() != null
                && mapping.dictionary().types() != null
                && value instanceof UnsignedInteger uint) {

            var typeMap = mapping.dictionary().types();

            var dictionary = typeMap.get(type);

            if (dictionary == null) {
                return null;
            }

            var decoded = dictionary.getValue(uint.getValue().intValueExact());

            if (decoded != null) {
                return decoded;
            }
        }
        return null;
    }
}
