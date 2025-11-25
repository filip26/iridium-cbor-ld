package com.apicatalog.cborld.mapping;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;


public class DynamicTermMap implements TermMap {

    final Map<String, Integer> terms;
    final Map<Integer, String> codes;

    int nextCode;

    protected DynamicTermMap(
            int lastCode,
            Map<String, Integer> terms,
            Map<Integer, String> codes) {
        this.nextCode = lastCode;
        this.terms = terms;
        this.codes = codes;
    }

    public static DynamicTermMap newMap() {
        return newMap(
                KeywordMap.TERM_TO_CODE,
                KeywordMap.CUSTOM_OFFSET);
    }

    public static DynamicTermMap newMap(Map<String, Integer> terms, int nextCode) {

        var codes = terms
                .entrySet()
                .stream()
                .collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey));

        return new DynamicTermMap(
                nextCode,
                new LinkedHashMap<>(terms),
                codes);
    }

    public DynamicTermMap add(Collection<String> terms) {
        terms
                .stream()
                .sorted()
                .forEach(this::add);

        return this;
    }

    public DynamicTermMap add(String key) {
        if (!terms.containsKey(key)) {
            codes.put(nextCode, key);
            terms.put(key, nextCode);
            nextCode += 2;
        }
        return this;
    }

    @Override
    public String getValue(Integer code) {
        return codes.get(code);
    }

    @Override
    public Integer getCode(String term) {
        return terms.get(term);
    }

    @Override
    public Iterator<Entry<String, Integer>> iterator() {
        return terms.entrySet().iterator();
    }
}
