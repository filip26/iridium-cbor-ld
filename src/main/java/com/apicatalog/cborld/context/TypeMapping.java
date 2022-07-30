package com.apicatalog.cborld.context;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class TypeMapping {

    private final JsonValue mapping;
    
    protected TypeMapping(JsonValue mapping) {
        this.mapping = mapping;
    }
    
    public Collection<String> getType(String term) {
        
        if (JsonUtils.isObject(mapping)) {
            JsonValue value =  mapping.asJsonObject().get(term);
            if (JsonUtils.isObject(value)) {
                return value
                            .asJsonObject()
                            .entrySet()
                            .stream()
                            .filter(e -> Keywords.TYPE.equals(e.getValue()))
                            .map(Map.Entry::getKey)
                            .collect(Collectors.toSet());
            }
            if (JsonUtils.isString(value)) {
                return Arrays.asList(((JsonString)value).getString());
            }
            if (JsonUtils.isArray(value)) {
                return value
                            .asJsonArray()
                            .stream()
                            .filter(JsonUtils::isString)
                            .map(JsonString.class::cast)
                            .map(JsonString::getString)
                            .collect(Collectors.toSet());
            }
        }
        return null;
    }
    
    public TypeMapping getMapping(String term) {
        if (JsonUtils.isObject(mapping)) {
            JsonValue value =  mapping.asJsonObject().get(term);
            if (JsonUtils.isObject(value)) {
                return new TypeMapping(value);
            }
        }
        return null;
    }
    
}
