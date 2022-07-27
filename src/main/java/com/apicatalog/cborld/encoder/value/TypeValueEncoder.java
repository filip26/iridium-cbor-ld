package com.apicatalog.cborld.encoder.value;

import com.apicatalog.cborld.dictionary.CodecTermMap;
import com.apicatalog.json.cursor.JsonValueCursor;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class TypeValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(CodecTermMap dictionary, JsonValueCursor value, String term, TermDefinition def) {
	if (def != null && Keywords.TYPE.equals(def.getUriMapping())) {
	    final Integer code = dictionary.getCode(value.stringValue());

	    if (code != null) {
		return new UnsignedInteger(code);
	    }
	}
	return null;
    }
}
