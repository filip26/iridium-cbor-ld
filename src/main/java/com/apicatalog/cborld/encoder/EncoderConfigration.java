package com.apicatalog.cborld.encoder;

import java.util.Collection;

import com.apicatalog.cborld.config.DictionaryAlgorithm;
import com.apicatalog.cborld.encoder.value.ValueEncoder;

public interface EncoderConfigration {

    boolean isCompactArrays();
    
    DictionaryAlgorithm getDictonaryAlgorithm();

    Collection<ValueEncoder> getValueEncoders();
}
