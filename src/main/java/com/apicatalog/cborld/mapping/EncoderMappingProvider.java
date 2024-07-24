package com.apicatalog.cborld.mapping;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.encoder.EncoderConfig;
import com.apicatalog.cursor.MapCursor;

public interface EncoderMappingProvider {

    Mapping getEncoderMapping(MapCursor document, EncoderConfig config) throws ContextError;
}
