package com.apicatalog.cborld.decoder;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import com.apicatalog.cborld.context.TypeMapper;
import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.JsonValue;

class TypeMapperImpl implements TypeMapper {

    final Map<String, Object> mapping;
    
    Map<String, Object> top;
    
    public TypeMapperImpl() {
        this.mapping = new HashMap<>();
        this.top = this.mapping;
    }
    
    @Override
    public void beginMap(String key) {
        System.out.println("begin " + key);
        
    }

    @Override
    public void typeKeyName(String type) {
        System.out.println(" @type = " + type);
        top.put(type, Keywords.TYPE);
    }

    @Override
    public void end() {
        System.out.println("end");
        
    }
    
    public Map<String, Object> getMapping(String term) {
        return mapping;
    }
    
}
