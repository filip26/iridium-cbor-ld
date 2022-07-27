package com.apicatalog.cborld.encoder.value;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.encoder.EncoderError;
import com.apicatalog.json.cursor.JsonValueCursor;
import com.apicatalog.jsonld.context.TermDefinition;

import co.nstant.in.cbor.model.DataItem;

public interface ValueEncoder {

    DataItem encode(Dictionary dictionary, JsonValueCursor value, String term, TermDefinition def) throws EncoderError;
    
}
