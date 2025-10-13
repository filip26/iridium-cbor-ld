package com.apicatalog.cborld.context.mapping;

import com.apicatalog.cborld.context.Context;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.context.ContextError.Code;
import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.tree.io.NodeAdapter;

import co.nstant.in.cbor.model.DataItem;

public class ContextMappingProvider implements EncoderMappingProvider, DecoderMappingProvider {

    @Override
    public Mapping getEncoderMapping(Object document, NodeAdapter adapter, Encoder encoder) throws ContextError {
        try {

            final Context context = Context.from(document,
                    adapter,
                    encoder.base(),
                    encoder.loader());

            return new EncoderContextMapping(
                    encoder.config().dictionary(),
                    CodeTermMap.of(context.getContextKeySets()),
                    context.getTypeMapping());

        } catch (JsonLdError e) {
            throw new ContextError(Code.InvalidContext, e);
        }
    }

    @Override
    public Mapping getDecoderMapping(DataItem document, DocumentDictionary dictionary, Decoder decoder) throws ContextError {
        try {
            var mapping = new DecoderContextMapping(dictionary, decoder.config().valueDecoders());

            var adapter = new CborMapping(
                    mapping::decodeTerm,
                    mapping::encodeTerm,
                    mapping::decodeValue);

            var context = Context.from(document,
                    adapter,
                    decoder.base(),
                    decoder.loader(),
                    mapping::add,
                    mapping.typeKeyNameMap());

            mapping.typeMap(context.getTypeMapping());

            return mapping;

        } catch (JsonLdError e) {
            throw new ContextError(Code.InvalidContext, e);
        }
    }
}
