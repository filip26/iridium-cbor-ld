package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class IdValueDecoder implements ValueDecoder {

    @Override
    public JsonValue decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderError {
        if (mapping != null
                && types != null
                && types.contains(Keywords.ID)
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())) {

            int code = ((UnsignedInteger) value).getValue().intValueExact();

            String id = mapping.dictionary() != null && mapping.dictionary().uris() != null
                    ? mapping.dictionary().uris().getValue(code)
                    : null;

            if (id == null && mapping.termMap() != null) {
                id = mapping.termMap().getValue(code);
            }

            if (id != null) {
                return Json.createValue(id);
            }
        }
        return null;
    }

}
