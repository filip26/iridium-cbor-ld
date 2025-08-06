package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class CustomTypeValueDecoder implements ValueDecoder {

    @Override
    public JsonValue decode(final Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderException {

        if (mapping != null
                && mapping.dictionary() != null
                && mapping.dictionary().types() != null
                && types != null
                && MajorType.UNSIGNED_INTEGER.equals(value.getMajorType())) {

            var typeMap = mapping.dictionary().types();

            for (final String type : types) {

                final Dictionary dictionary = typeMap.get(type);

                if (dictionary == null) {
                    continue;
                }
                final String decoded = dictionary.getValue(((UnsignedInteger) value).getValue().intValueExact());
                if (decoded != null) {
                    return Json.createValue(decoded);
                }
            }
        }
        return null;
    }
}
