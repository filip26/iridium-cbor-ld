package com.apicatalog.cborld.mapping;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.encoder.EncoderConfig;

import jakarta.json.JsonObject;

public interface EncoderMappingProvider {

    Mapping getEncoderMapping(JsonObject document, EncoderConfig config) throws ContextError;
}
