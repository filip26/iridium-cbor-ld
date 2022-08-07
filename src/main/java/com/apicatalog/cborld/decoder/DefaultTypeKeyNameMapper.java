package com.apicatalog.cborld.decoder;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;

import com.apicatalog.cborld.context.TypeKeyNameMapper;

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
            List<String> reversed = new ArrayList<>(path);
            Collections.reverse(reversed);  //TODO
            index =  String.join(".", reversed) + "." + type;
        }
        
        paths.add(index);
    }

    @Override
    public void end() {
        path.pop();
    }
    
    public boolean isTypeKey(String pointer) {
        System.out.println(pointer + ", " + paths);
        return paths.contains(pointer);
    }
    
}
