package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.encoder.EncoderError;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cursor.ValueCursor;

import co.nstant.in.cbor.model.DataItem;

public class CustomTypeValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Mapping mapping, ValueCursor value, String term, Collection<String> types) throws EncoderError {
        // TODO Auto-generated method stub
        return null;
    }

}
