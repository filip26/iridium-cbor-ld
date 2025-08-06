package com.apicatalog.cborld.dictionary;

import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

record BidirectionalDictionary(
        Map<String, Integer> index,
        Map<Integer, String> reverse) implements Dictionary {

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