package com.apicatalog.cborld.mapping.context;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;

import com.apicatalog.cborld.mapping.TypeKeyNameMapper;
import com.apicatalog.jsonld.lang.Keywords;

class DefaultTypeKeyNameMapper implements TypeKeyNameMapper {

    final Deque<Collection<String>> typeKeys;

    public DefaultTypeKeyNameMapper() {
        this.typeKeys = new ArrayDeque<>(10);
        typeKeys.push(List.of());
    }

    @Override
    public void beginMap(String key) {
        typeKeys.push(List.of());
    }

    @Override
    public void typeKey(String key) {

        if (typeKeys.peek().isEmpty()) {
            typeKeys.pop();
            typeKeys.push(new HashSet<>());
        }

        typeKeys.peek().add(key);
    }

    @Override
    public void endMap() {
        typeKeys.pop();
    }

    @Override
    public boolean isTypeKey(String term) {
        return Keywords.TYPE.equals(term)
                || (!typeKeys.isEmpty() && typeKeys.peek().contains(term));
    }
}
