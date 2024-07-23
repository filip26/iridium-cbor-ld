package com.apicatalog.cborld.context.mapping;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.HashSet;

class DefaultTypeKeyNameMapper implements TypeKeyNameMapper {

    final Collection<String> paths;
    
    final Deque<String> path;
    
    public DefaultTypeKeyNameMapper() {
        this.paths = new HashSet<>();
        this.path = new ArrayDeque<>(10);
    }
    
    @Override
    public void beginMap(String key) {
        path.push(key);
    }

    @Override
    public void typeKeyName(String type) {
        
        final String index;
        
        if (path.isEmpty()) {
            index = type;
            
        } else {
            index =  pointer(type, path);
        }
        
        paths.add(index);
    }

    @Override
    public void end() {
        path.pop();
    }

    @Override
    public boolean isTypeKey(String term, Collection<String> termPath) {
        return paths.contains(pointer(term, termPath));
    }
    
    static final String pointer(String term, Collection<String> path) {
        return term + ((path == null || path.isEmpty()) ? "" : "." + String.join(".", path));
    }
    
}
