package com.apicatalog.cborld.context.mapping;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.mapping.TypeMap;

class ContextEncoderMapping implements Mapping {

    private final Dictionary dictionary;

    private final TypeMap typeMap;
        
    ContextEncoderMapping(Dictionary dictionary, TypeMap typeMap) {
        this.dictionary = dictionary;
        this.typeMap = typeMap;
    }

    @Override
    public Dictionary dictionary() {
        return dictionary;
    }

    @Override
    public TypeMap typeMap() {
        return typeMap;
    }
}
