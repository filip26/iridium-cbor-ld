package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.encoder.EncoderError;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cursor.ValueCursor;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class CustomTypeValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Mapping mapping, ValueCursor value, String term, Collection<String> types) throws EncoderError {
        
        if (types != null) {
            for (final String type : types) {
                
                final Dictionary dictionary = mapping.type(type);

                if (dictionary == null) {
                    continue;
                }
                final Integer code = dictionary.getCode(value.stringValue());
                if (code != null) {
                    return new UnsignedInteger(code);
                }

            }            
        }
        
        return null;
    }

}
