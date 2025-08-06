package com.apicatalog.cborld.encoder;

import java.net.URI;

import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.jsonld.loader.DocumentLoader;

import jakarta.json.JsonObject;

public class DebugEncoder {

    final EncoderConfig config;
    final EncoderMappingProvider provider;
    final DocumentLoader loader;
    final URI base;
    
    public DebugEncoder(EncoderConfig config, EncoderMappingProvider provider, DocumentLoader loader, URI base) {
        this.config = config;
        this.provider = provider;
        this.loader = loader;
        this.base = base;
    }

    public void encode(JsonObject object) {
        // TODO Auto-generated method stub
        
    }

    public JsonObject dump() {
        // TODO Auto-generated method stub
        return null;
    }

}
