package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.dictionary.Dictionary;

import co.nstant.in.cbor.model.DataItem;
import jakarta.json.JsonValue;

public interface ValueDecoder {

    JsonValue decode(Dictionary dictionary, DataItem value, String term, Collection<String> types) throws DecoderError; 
}
