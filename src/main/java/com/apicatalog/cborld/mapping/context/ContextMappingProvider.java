package com.apicatalog.cborld.mapping.context;

import java.util.Collection;
import java.util.LinkedHashSet;

import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.decoder.DecoderException.DecoderError;
import com.apicatalog.cborld.dictionary.TermMap;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.cborld.encoder.EncoderException.EncoderError;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.tree.io.TreeAdapter;

import co.nstant.in.cbor.model.DataItem;

public class ContextMappingProvider implements EncoderMappingProvider, DecoderMappingProvider {

    @Override
    public Mapping getEncoderMapping(Object document, TreeAdapter adapter, Encoder encoder) throws EncoderException {
        try {

            final var appliedContextKeys = new LinkedHashSet<Collection<String>>();

            final var typeMap = TypeMapImpl.newTypeMap(
                    document,
                    adapter,
                    encoder.base(),
                    encoder.loader(),
                    appliedContextKeys::add,
                    null);

            final var termMap = TermMap.newMap();

            appliedContextKeys.forEach(termMap::add);

            return new EncoderContextMapping(
                    encoder.config().dictionary(),
                    termMap,
                    typeMap);

        } catch (JsonLdException e) {
            if (ErrorCode.INLINE_CONTEXT_IS_NOT_ALLOWED == e.code()) {
                throw new EncoderException(EncoderError.NON_COMPRESSIBLE, e);
            }

            throw new EncoderException(EncoderError.INVALID_CONTEXT, e);
        }
    }

    @Override
    public Mapping getDecoderMapping(DataItem document, DocumentDictionary dictionary, Decoder decoder) throws DecoderException {
        try {
            final var mapping = new DecoderContextMapping(dictionary, decoder.config().valueDecoders());

            final var adapter = new CborLdAdapter(
                    mapping::decodeTerm,
                    mapping::encodeTerm,
                    mapping::decodeValue);

            final var typeMap = TypeMapImpl.newTypeMap(
                    document,
                    adapter,
                    decoder.base(),
                    decoder.loader(),
                    mapping::add,
                    mapping.typeKeyNameMap());

            mapping.typeMap(typeMap);

            return mapping;

        } catch (JsonLdException e) {
            throw new DecoderException(DecoderError.INVALID_CONTEXT, e);
        }
    }
}
