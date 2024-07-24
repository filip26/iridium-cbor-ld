package com.apicatalog.cborld.decoder;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.cborld.config.Config;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.document.DocumentDictionary;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;

public interface DecoderConfig  extends Config {

    Map<Integer, DocumentDictionary> dictionaries();

    Collection<ValueDecoder> valueDecoders();
    
    DecoderMappingProvider decoderMapping();
}
