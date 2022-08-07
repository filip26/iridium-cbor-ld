package com.apicatalog.cborld.db;

import java.net.URI;

import com.apicatalog.cborld.context.Context;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.context.ContextError.Code;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.encoder.EncoderConfig;
import com.apicatalog.cborld.mapper.Mapping;
import com.apicatalog.cborld.mapper.MappingProvider;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.cbor.CborCursor;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.model.DataItem;

public class DbMappingProvider implements MappingProvider {

    @Override
    public Mapping getEncoderMapping(MapCursor document, URI base, DocumentLoader loader, EncoderConfig config) throws ContextError {
        try {
            final Context context = Context.from(document, base, loader);

            return new DbEncoderMapping(
                        CodeTermMap.from(context.getContextKeySets(), loader),
                        context.getTypeMapping()
                        );
            
        } catch (JsonLdError e) {
            throw new ContextError(Code.InvalidContext, e);
        }
    }

    @Override
    public Mapping getDecoderMapping(DataItem document, URI base, DocumentLoader loader, DecoderConfig config) throws ContextError {
        try {
            final DbDecoderMapping mapping = new DbDecoderMapping();
            mapping.valueDecoders(config.valueDecoders());
    
            final MapCursor cursor = CborCursor.from(
                    document, 
                    mapping::decodeKey, 
                    mapping::encodeKey,
                    mapping::decodeValue
                    );
    
            final Context context = Context.from(cursor, base, loader, mapping::add, mapping.typeKeyNameMap());
            
            mapping.typeMap(context.getTypeMapping());
            
            return mapping;
            
        } catch (JsonLdError e) {
            throw new ContextError(Code.InvalidContext, e);
        }
    }
}
