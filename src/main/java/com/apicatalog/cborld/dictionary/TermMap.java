package com.apicatalog.cborld.dictionary;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

public class TermMap implements Dictionary {

    final Map<String, Integer> terms;
    final Map<Integer, String> codes;

    int lastCode;

    protected TermMap(
            int lastCode,
            Map<String, Integer> terms,
            Map<Integer, String> codes) {
        this.lastCode = lastCode;
        this.terms = terms;
        this.codes = codes;
    }

    public static TermMap newMap() {
        return newMap(
                KeywordDictionary.TERM_TO_CODE,
                KeywordDictionary.CUSTOM_OFFSET);
    }

    public static TermMap newMap(Map<String, Integer> terms, int lastCode) {

        var codes = terms
                .entrySet()
                .stream()
                .collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey));

        return new TermMap(
                lastCode,
                new LinkedHashMap<>(terms),
                codes);
    }

    public TermMap add(Collection<String> terms) {
        terms
                .stream()
                .sorted()
                .forEach(this::add);

        return this;
    }

    public TermMap add(String key) {
        if (!terms.containsKey(key)) {
            codes.put(lastCode, key);
            terms.put(key, lastCode);
            lastCode += 2;
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
