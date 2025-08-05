package com.apicatalog.cborld.mapping;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.encoder.Encoder;

import jakarta.json.JsonObject;

public interface EncoderMappingProvider {

    Mapping getEncoderMapping(JsonObject document, Encoder encoder) throws ContextError;
}
