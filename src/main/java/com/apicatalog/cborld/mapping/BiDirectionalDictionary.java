package com.apicatalog.cborld.mapping;

import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

//TODO use builder
@Deprecated
record BiDirectionalDictionary(
        Map<String, Integer> index,
        Map<Integer, String> reverse) implements TermMap {

    BiDirectionalDictionary {
        index = index != null ? Map.copyOf(index) : Map.of();
        reverse = reverse != null ? Map.copyOf(reverse) : Map.of();
    }
    
    @Override
    public Integer getCode(String value) {
        return index.get(value);
    }

    @Override
    public String getValue(Integer code) {
        return reverse.get(code);
    }

    @Override
    public Iterator<Entry<String, Integer>> iterator() {
        return index.entrySet().iterator();
    }
}