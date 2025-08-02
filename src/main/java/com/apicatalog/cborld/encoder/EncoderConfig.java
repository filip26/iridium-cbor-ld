package com.apicatalog.cborld.encoder;

import java.util.Collection;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.config.Config;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.cborld.registry.DocumentDictionary;

public interface EncoderConfig extends Config {

    CborLdVersion version();

    DocumentDictionary dictionary();

    boolean isCompactArrays();
    
    Collection<ValueEncoder> valueEncoders();

    EncoderMappingProvider encoderMapping();
}
