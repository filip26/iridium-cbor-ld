package com.apicatalog.cborld.db;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.mapper.Mapping;
import com.apicatalog.cborld.mapper.TypeMap;

class DbEncoderMapping implements Mapping {

    private final Dictionary dictionary;

    private final TypeMap typeMap;
        
    DbEncoderMapping(Dictionary dictionary, TypeMap typeMap) {
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
