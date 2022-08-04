package com.apicatalog.cborld.decoder;

import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;

import co.nstant.in.cbor.model.DataItem;
import jakarta.json.JsonValue;

public interface ValueDecoder {

    JsonValue decode(Dictionary dictionary, DataItem value, String term, Collection<String> types) throws DecoderError; 
}
