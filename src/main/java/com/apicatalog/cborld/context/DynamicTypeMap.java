package com.apicatalog.cborld.context;

import java.util.Collection;
import java.util.Collections;

import com.apicatalog.cborld.mapping.TypeMap;

import jakarta.json.JsonValue;

class DynamicTypeMap implements TypeMap {

    private final JsonValue mapping;

    public DynamicTypeMap(JsonValue mapping) {
        System.out.println("CB2- TM " + mapping);
        this.mapping = mapping;
    }

    @Override
    public Collection<String> getType(String term) {

//        if (JsonUtils.isObject(mapping)) {
//            JsonValue value = mapping.asJsonObject().get(term);
//            if (JsonUtils.isObject(value)) {
//                return value
//                        .asJsonObject()
//                        .entrySet()
//                        .stream()
//                        .filter(e -> Keywords.TYPE.equals(e.getKey()))
//                        .map(Map.Entry::getValue)
//                        .filter(JsonUtils::isString)
//                        .map(JsonString.class::cast)
//                        .map(JsonString::getString)
//                        .collect(Collectors.toSet());
//            }
//            if (JsonUtils.isString(value)) {
//                return Arrays.asList(((JsonString) value).getString());
//            }
//            if (JsonUtils.isArray(value)) {
//                return value
//                        .asJsonArray()
//                        .stream()
//                        .filter(JsonUtils::isString)
//                        .map(JsonString.class::cast)
//                        .map(JsonString::getString)
//                        .collect(Collectors.toSet());
//            }
//        }
        return Collections.emptyList();
    }

    @Override
    public DynamicTypeMap getMapping(String term) {
//        System.out.println("MAP " + term );
//        if (JsonUtils.isObject(mapping)) {
//            JsonValue value = mapping.asJsonObject().get(term);
//            if (JsonUtils.isObject(value)) {
//                return new DynamicTypeMap(value);
//            }
//        }
        return null;
    }
    
    //TODO remove
    @Override
    public String toString() {
        return mapping.toString();
        
    }
    
}
