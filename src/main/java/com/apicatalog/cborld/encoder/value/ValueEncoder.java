package com.apicatalog.cborld.encoder.value;

import com.apicatalog.cborld.dictionary.CodecTermMap;
import com.apicatalog.json.cursor.JsonValueCursor;
import com.apicatalog.jsonld.context.TermDefinition;

import co.nstant.in.cbor.model.DataItem;

public interface ValueEncoder {

    DataItem encode(CodecTermMap dictionary, JsonValueCursor value, String term, TermDefinition def);
    
}
