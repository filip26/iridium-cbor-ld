package com.apicatalog.cborld.context.mapping;

import java.net.URI;

import com.apicatalog.cborld.context.Context;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.context.ContextError.Code;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.encoder.EncoderConfig;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.loader.DocumentLoader;

public class ContextEncoderMappingProvider implements EncoderMappingProvider {

    @Override
    public Mapping getEncoderMapping(MapCursor document, URI base, DocumentLoader loader, EncoderConfig config) throws ContextError {
        try {
            final Context context = Context.from(document, base, loader);

            return new ContextEncoderMapping(
                        CodeTermMap.from(context.getContextKeySets(), loader),
                        context.getTypeMapping()
                        );
            
        } catch (JsonLdError e) {
            throw new ContextError(Code.InvalidContext, e);
        }
    }
}
