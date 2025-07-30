package com.apicatalog.cborld.context.mapping;

import com.apicatalog.cborld.context.Context;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.context.ContextError.Code;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.document.DocumentDictionary;
import com.apicatalog.cborld.encoder.EncoderConfig;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.lq.ValueHolder;
import com.apicatalog.lq.jakarta.JakartaAdapter;

import co.nstant.in.cbor.model.DataItem;
import jakarta.json.JsonObject;

public class ContextMappingProvider implements EncoderMappingProvider, DecoderMappingProvider {

    @Override
    public Mapping getEncoderMapping(JsonObject document, EncoderConfig config) throws ContextError {
        try {

            final ValueHolder value = JakartaAdapter.of(document);

            final Context context = Context.from(value, config.base(), config.loader());

            return new EncoderContextMapping(
                    config.dictionary().contexts(),
                    config.dictionary().types(),
                    CodeTermMap.of(context.getContextKeySets()),
                    context.getTypeMapping());

        } catch (JsonLdError e) {
            throw new ContextError(Code.InvalidContext, e);
        }
    }

    @Override
    public Mapping getDecoderMapping(DataItem document, DocumentDictionary custom, DecoderConfig config) throws ContextError {
//        try {
            final DecoderContextMapping mapping = new DecoderContextMapping(custom.contexts(), custom.types(), config.valueDecoders());

//            final MapCursor cursor = CborCursor.from(
//                    document,
//                    mapping::decodeKey,
//                    mapping::encodeKey,
//                    mapping::decodeValue);
//
//            final Context context = Context.from(cursor, config.base(), config.loader(), mapping::add, mapping.typeKeyNameMap());
//
//            mapping.typeMap(context.getTypeMapping());

            return mapping;

//        } catch (JsonLdError e) {
//            throw new ContextError(Code.InvalidContext, e);
//        }
    }
}
