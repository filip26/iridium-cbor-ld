package com.apicatalog.cborld.decoder;

import java.util.Collection;

import com.apicatalog.cborld.config.Config;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;

public interface DecoderConfig  extends Config {

    Collection<ValueDecoder> valueDecoders();
    
    DecoderMappingProvider mapping();
}
