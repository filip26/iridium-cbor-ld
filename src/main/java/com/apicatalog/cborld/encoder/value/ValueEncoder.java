package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.encoder.EncoderError;
import com.apicatalog.json.cursor.JsonValueCursor;

import co.nstant.in.cbor.model.DataItem;

public interface ValueEncoder {

    DataItem encode(Dictionary dictionary, JsonValueCursor value, String term, Collection<String> types) throws EncoderError;

}
