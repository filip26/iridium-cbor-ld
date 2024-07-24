package com.apicatalog.cborld.decoder;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.cborld.config.Config;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.dictionary.CustomDictionary;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;

public interface DecoderConfig  extends Config {

    Map<Integer, CustomDictionary> dictionaries();

    Collection<ValueDecoder> valueDecoders();
    
    DecoderMappingProvider decoderMapping();
}
