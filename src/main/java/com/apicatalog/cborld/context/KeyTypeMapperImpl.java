package com.apicatalog.cborld.context;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.jsonld.processor.KeyTypeMapper;

public class KeyTypeMapperImpl implements KeyTypeMapper {

    final Deque<Map<String, Object>> stack;

    public KeyTypeMapperImpl() {
        this.stack = new ArrayDeque<>();
        this.stack.push(new LinkedHashMap<String, Object>());
    }

    @Override
    public void beginMap(String key) {
        var map = new LinkedHashMap<String, Object>();
        stack.peek().put(key, map);
        stack.push(map);
    }

    @Override
    public void endMap() {
        stack.pop();
    }

    @Override
    public void mapProperty(String key, String id) {
        stack.peek().put(key, id);
    }

    @SuppressWarnings("unchecked")
    @Override
    public void mapProperty(String key, String type, String value) {
        ((Map<String, Object>) stack.peek()
                .computeIfAbsent(key, (k) -> new LinkedHashMap<String, Object>()))
                .put(type, value);
    }

    public TypeMap typeMap() {
        return new KeyTypeMapImpl(stack.peek());
    }
}
