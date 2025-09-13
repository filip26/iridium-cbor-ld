package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;

public class TypeValueDecoder implements ValueDecoder {

    @Override
    public String decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderException {
        if (mapping != null
                && mapping.termMap() != null
                && types != null
                && types.contains(Keywords.TYPE)
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())) {
            return mapping.termMap().getValue(((UnsignedInteger) value).getValue().intValueExact());
        }
        return null;
    }
}
