package com.apicatalog.cborld.decoder;


import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.registry.DocumentDictionary;

import co.nstant.in.cbor.model.DataItem;

@FunctionalInterface
public interface DecoderMappingProvider {

    Mapping getDecoderMapping(DataItem document, DocumentDictionary dictionary, Decoder decoder) throws DecoderException;
}
