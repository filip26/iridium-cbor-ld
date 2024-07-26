package com.apicatalog.cborld.dictionary;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

public class DictionaryBuilder {

    protected DoubleIndexDictionary dictionary;

    protected DictionaryBuilder() {
        this.dictionary = new DoubleIndexDictionary();
    }

    protected DictionaryBuilder(DoubleIndexDictionary dictionary) {
        this.dictionary = dictionary;
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
        for (Map.Entry<Integer, String> entry : dictionary) {
            set(entry.getKey(), entry.getValue());
        }
        return this;
    }

    public DictionaryBuilder set(Integer code, String value) {
        dictionary.set(code, value);
        return this;
    }

    public DictionaryBuilder set(String value, Integer code) {
        dictionary.set(code, value);
        return this;
    }

    public Dictionary build() {
        return dictionary;
    }

    class DoubleIndexDictionary implements Dictionary {

        protected final Map<String, Integer> index = new LinkedHashMap<>();
        protected final Map<Integer, String> reverse = new LinkedHashMap<>();

        protected void set(Integer code, String value) {
            index.put(value, code);
            reverse.put(code, value);
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
