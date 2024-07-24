package com.apicatalog.cborld.mapping;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.dictionary.CustomDictionary;

import co.nstant.in.cbor.model.DataItem;

public interface DecoderMappingProvider {

    Mapping getDecoderMapping(DataItem document, CustomDictionary custom, DecoderConfig config) throws DecoderError, ContextError;
}
