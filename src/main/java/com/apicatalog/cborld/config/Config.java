package com.apicatalog.cborld.config;

import com.apicatalog.cborld.mapper.MappingProvider;

public interface Config {

    boolean isCompactArrays();
    
    DictionaryAlgorithm dictonaryAlgorithm();
    
    MappingProvider provider();
}
