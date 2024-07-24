package com.apicatalog.cborld.encoder;

import java.util.Collection;

import com.apicatalog.cborld.config.Config;
import com.apicatalog.cborld.dictionary.DocumentDictionary;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;

public interface EncoderConfig extends Config {

    DocumentDictionary dictionary();
    
    byte version();
    
    Collection<ValueEncoder> valueEncoders();

    EncoderMappingProvider encoderMapping();
}
