package com.apicatalog.cborld.mapper;

import java.net.URI;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.encoder.EncoderConfig;
import com.apicatalog.cborld.encoder.EncoderError;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.model.DataItem;


public interface MappingProvider {
    
    Mapping getEncoderMapping(MapCursor document, URI base, DocumentLoader loader, EncoderConfig config) throws EncoderError, ContextError;
    
    //TODO use MapCursor
    Mapping getDecoderMapping(DataItem document, URI base, DocumentLoader loader, DecoderConfig config) throws DecoderError, ContextError;
    
}
