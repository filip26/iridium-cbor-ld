package com.apicatalog.cborld.dictionary;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

public class DictionaryBuilder {

    protected final Map<String, Integer> index;
    protected final Map<Integer, String> reverse;

    protected DictionaryBuilder() {
        this.index = new LinkedHashMap<>();
        this.reverse = new LinkedHashMap<>();
    }

    protected DictionaryBuilder(DoubleIndexDictionary dictionary) {
        this.index = new LinkedHashMap<>(dictionary.index);
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
        index.put(value, code);
        reverse.put(code, value);
        return this;
    }

    public DictionaryBuilder set(String value, Integer code) {
        index.put(value, code);
        reverse.put(code, value);
        return this;
    }

    public Dictionary build() {
        return new DoubleIndexDictionary(
                Collections.unmodifiableMap(index),
                Collections.unmodifiableMap(reverse));
    }

    class DoubleIndexDictionary implements Dictionary {

        protected final Map<String, Integer> index;
        protected final Map<Integer, String> reverse;

        public DoubleIndexDictionary(Map<String, Integer> index, Map<Integer, String> reverse) {
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
