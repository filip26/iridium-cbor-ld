package com.apicatalog.cborld.decoder.value;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class IdValueDecoder implements ValueDecoder {

    @Override
    public String decode(Mapping mapping, DataItem value, String term, String type) throws DecoderException {
        if ((Keywords.ID.equals(type) || Keywords.ID.equals(term))
                && mapping != null
                && value instanceof UnsignedInteger uint) {

            var code = uint.getValue().intValueExact();

            var id = mapping.dictionary() != null && mapping.dictionary().uris() != null
                    ? mapping.dictionary().uris().getValue(code)
                    : null;

            if (id == null && mapping.termMap() != null) {
                id = mapping.termMap().getValue(code);
            }

            return id;
        }
        return null;
    }

}
