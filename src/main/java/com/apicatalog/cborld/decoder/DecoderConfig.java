package com.apicatalog.cborld.decoder;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.config.Config;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.registry.DocumentDictionary;

public interface DecoderConfig extends Config {

    CborLdVersion version();

    boolean isCompactArrays();
    
    Map<Integer, DocumentDictionary> registry();

    Collection<ValueDecoder> valueDecoders();

    DecoderMappingProvider decoderMapping();

}
