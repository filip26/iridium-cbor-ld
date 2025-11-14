package com.apicatalog.cborld.context;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.jsonld.lang.Keywords;

public class KeyTypeMapImpl implements TypeMap {

    final Map<String, Object> typeMap;

    KeyTypeMapImpl(final Map<String, Object> typeMap) {
        this.typeMap = typeMap;
    }

    @Override
    public Collection<String> getType(String term) {
        System.out.println("GET T: " + term);
        System.out.println("     : " + typeMap);

        var type = typeMap.get(term);
        if (type instanceof Map map) {
            type = map.get(Keywords.TYPE);
        } 
        
        if (type instanceof String string) {
            return List.of(string);
            
        } else if (type instanceof Collection array) {
            return (Collection<String>) array;
        }

        return List.of();
//        return Optional.ofNullable()
//                .map(Map.class::cast)
//                .map(m -> m.get(Keywords.TYPE))
//                .map(String.class::cast)
//                .map(List::of)
//                .orElse(List.of())
//                ;
    }

    @Override
    public TypeMap getMapping(String term) {
        System.out.println("GET M: " + term);
        System.out.println("     : " + typeMap);
        
        var type = typeMap.get(term);
        if (type instanceof Map map) {
            return new KeyTypeMapImpl(map);
        }
        return null;
    }
}
