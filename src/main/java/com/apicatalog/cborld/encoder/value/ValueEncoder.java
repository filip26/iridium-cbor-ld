package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import jakarta.json.JsonValue;

public interface ValueEncoder {

    DataItem encode(Mapping mapping, JsonValue jsonValue, String term, Collection<String> types) throws EncoderException;

}
