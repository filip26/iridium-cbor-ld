package com.apicatalog.cborld.context.mapping;

import java.util.Map;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.mapping.TypeMap;

class EncoderContextMapping implements Mapping {

    private final Dictionary contexts;
    private final Map<String, Dictionary> types;
    private final Dictionary uris;
    
    private final Dictionary dictionary;
    private final TypeMap typeMap;

    //TODO revise
    EncoderContextMapping(Dictionary contexts, Map<String, Dictionary> types, Dictionary uris, Dictionary dictionary, TypeMap typeMap) {
        this.contexts = contexts;
        this.types = types;
        this.uris = uris;
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
    public Dictionary contexts() {
        return contexts;
    }

    @Override
    public Dictionary type(String type) {
        return types != null ? types.get(type) : null;
    }

    @Override
    public Dictionary uris() {
        return uris;
    }
}
