package com.apicatalog.cborld.dictionary;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

public class DictionaryBuilder {

    protected final Map<Integer, String> reverse;

    protected DictionaryBuilder() {
        this.reverse = new LinkedHashMap<>();
    }

    protected DictionaryBuilder(DoubleIndexDictionary dictionary) {
        this.reverse = new LinkedHashMap<>(dictionary.reverse);
    }

    public static DictionaryBuilder create() {
        return new DictionaryBuilder();
    }

    public static DictionaryBuilder of(Dictionary dictionary) {
        if (dictionary instanceof DoubleIndexDictionary) {
            return new DictionaryBuilder((DoubleIndexDictionary) dictionary);
        }
        return new DictionaryBuilder().merge(dictionary);
    }

    public DictionaryBuilder merge(Dictionary dictionary) {
        dictionary.forEach(entry -> set(entry.getKey(), entry.getValue()));
        return this;
    }

    public DictionaryBuilder set(Integer code, String value) {
        reverse.put(code, value);
        return this;
    }

    public DictionaryBuilder set(String value, Integer code) {
        reverse.put(code, value);
        return this;
    }

    public Dictionary build() {
        return new DoubleIndexDictionary(
                reverse.entrySet()
                        .stream()
                        .collect(Collectors.toUnmodifiableMap(Entry::getValue, Entry::getKey)),
                Collections.unmodifiableMap(reverse));
    }

    class DoubleIndexDictionary implements Dictionary {

        final Map<String, Integer> index;
        final Map<Integer, String> reverse;

        DoubleIndexDictionary(Map<String, Integer> index, Map<Integer, String> reverse) {
            this.index = index;
            this.reverse = reverse;
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
        public Iterator<Entry<Integer, String>> iterator() {
            return reverse.entrySet().iterator();
        }
    }
}
