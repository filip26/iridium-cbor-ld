package com.apicatalog.cborld.decoder;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.cborld.config.BaseConfig;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.registry.DocumentDictionary;

public interface DecoderConfig extends BaseConfig {
    
    Map<Integer, DocumentDictionary> registry();

    Collection<ValueDecoder> valueDecoders();

    DecoderMappingProvider decoderMapping();

}
