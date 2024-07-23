package com.apicatalog.cborld.compressor;

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
    public Dictionary terms() {
        return dictionary;
    }

    @Override
    public TypeMap typeMap() {
        return typeMap;
    }

    @Override
    public Dictionary context() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Dictionary type(String type) {
        // TODO Auto-generated method stub
        return null;
    }
}
