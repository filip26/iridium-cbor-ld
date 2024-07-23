package com.apicatalog.cborld.mapping;

import java.net.URI;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.encoder.EncoderConfig;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.jsonld.loader.DocumentLoader;

public interface EncoderMappingProvider {

    Mapping getEncoderMapping(MapCursor document, URI base, DocumentLoader loader, EncoderConfig config) throws ContextError;
}
