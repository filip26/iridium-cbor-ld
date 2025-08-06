package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class TypeValueDecoder implements ValueDecoder {

    @Override
    public JsonValue decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderException {
        if (mapping != null && mapping.termMap() != null && types != null && types.contains(Keywords.TYPE)
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())) {
            final String type = mapping.termMap().getValue(((UnsignedInteger) value).getValue().intValueExact());

            if (type != null) {
                return Json.createValue(type);
            }
        }
        return null;
    }
}
