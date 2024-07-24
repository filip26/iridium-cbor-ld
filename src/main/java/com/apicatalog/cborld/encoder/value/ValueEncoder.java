package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.encoder.EncoderError;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cursor.ValueCursor;

import co.nstant.in.cbor.model.DataItem;

public interface ValueEncoder {

    DataItem encode(Mapping mapping, ValueCursor value, String term, Collection<String> types) throws EncoderError;

}
