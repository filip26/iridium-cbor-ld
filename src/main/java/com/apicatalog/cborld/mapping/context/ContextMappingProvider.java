package com.apicatalog.cborld.mapping.context;

import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.mapping.context.ContextMappingException.Code;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.tree.io.TreeAdapter;

import co.nstant.in.cbor.model.DataItem;

public class ContextMappingProvider implements EncoderMappingProvider, DecoderMappingProvider {

    @Override
    public Mapping getEncoderMapping(Object document, TreeAdapter adapter, Encoder encoder) throws ContextMappingException {
        try {

            final ContextMap context = ContextMap.from(document,
                    adapter,
                    encoder.base(),
                    encoder.loader());

            return new EncoderContextMapping(
                    encoder.config().dictionary(),
                    CodeTermMap.of(context.getContextKeySets()),
                    context.getTypeMapping());

        } catch (JsonLdException e) {
            throw new ContextMappingException(Code.InvalidContext, e);
        }
    }

    @Override
    public Mapping getDecoderMapping(DataItem document, DocumentDictionary dictionary, Decoder decoder) throws ContextMappingException {
        try {
            var mapping = new DecoderContextMapping(dictionary, decoder.config().valueDecoders());

            var adapter = new CborLdAdapter(
                    mapping::decodeTerm,
                    mapping::encodeTerm,
                    mapping::decodeValue);

            var context = ContextMap.from(document,
                    adapter,
                    decoder.base(),
                    decoder.loader(),
                    mapping::add,
                    mapping.typeKeyNameMap());

            mapping.typeMap(context.getTypeMapping());

            return mapping;

        } catch (JsonLdException e) {
            throw new ContextMappingException(Code.InvalidContext, e);
        }
    }
}
