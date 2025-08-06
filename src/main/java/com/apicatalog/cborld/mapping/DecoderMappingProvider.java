package com.apicatalog.cborld.mapping;


import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.registry.DocumentDictionary;

import co.nstant.in.cbor.model.DataItem;

public interface DecoderMappingProvider {

    Mapping getDecoderMapping(DataItem document, DocumentDictionary dictionary, Decoder decoder) throws DecoderException, ContextError;
}
