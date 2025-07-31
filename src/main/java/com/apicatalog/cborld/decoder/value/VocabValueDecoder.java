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

public class VocabValueDecoder implements ValueDecoder {

    @Override
    public JsonValue decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderError {

        if (mapping != null && mapping.terms() != null && types != null && types.contains(Keywords.VOCAB)
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())) {

            String termValue = mapping.terms().getValue(((UnsignedInteger) value).getValue().intValueExact());

            if (termValue != null) {
                return Json.createValue(termValue);
            }
        }
        return null;
    }
}
