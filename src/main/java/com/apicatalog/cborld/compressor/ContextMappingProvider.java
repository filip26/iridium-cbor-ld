package com.apicatalog.cborld.compressor;

import java.net.URI;

import com.apicatalog.cborld.context.Context;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.context.ContextError.Code;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.encoder.EncoderConfig;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.cbor.CborCursor;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.model.DataItem;

public class ContextMappingProvider implements EncoderMappingProvider, DecoderMappingProvider {

    @Override
    public Mapping getEncoderMapping(MapCursor document, URI base, DocumentLoader loader, EncoderConfig config) throws ContextError {
        try {
            final Context context = Context.from(document, base, loader);

            return new ContextEncoderMapping(
                    CodeTermMap.from(context.getContextKeySets(), loader),
                    context.getTypeMapping());

        } catch (JsonLdError e) {
            throw new ContextError(Code.InvalidContext, e);
        }
    }

    @Override
    public Mapping getDecoderMapping(DataItem document, URI base, DocumentLoader loader, DecoderConfig config) throws ContextError {
        try {
            final ContextDecoderMapping mapping = new ContextDecoderMapping(config.contexts(), config.types());
            mapping.valueDecoders(config.valueDecoders());

            final MapCursor cursor = CborCursor.from(
                    document,
                    mapping::decodeKey,
                    mapping::encodeKey,
                    mapping::decodeValue);

            final Context context = Context.from(cursor, base, loader, mapping::add, mapping.typeKeyNameMap());

            mapping.typeMap(context.getTypeMapping());

            return mapping;

        } catch (JsonLdError e) {
            throw new ContextError(Code.InvalidContext, e);
        }
    }

}
