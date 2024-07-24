package com.apicatalog.cborld.dictionary;

import java.util.LinkedHashMap;
import java.util.Map;

public class DictionaryBuilder {

    protected DoubleIndexDictionary dictionary;
    
    protected DictionaryBuilder() {
        this.dictionary = new DoubleIndexDictionary();
    }
    
    public static DictionaryBuilder create() {
        return new DictionaryBuilder();
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
        
        protected final Map<String, Integer> INDEX = new LinkedHashMap<>();
        protected final Map<Integer, String> REVERSE = new LinkedHashMap<>();

        protected void set(Integer code, String value) {
            INDEX.put(value, code);
            REVERSE.put(code, value);
        }
        
        @Override
        public Integer getCode(String value) {
            return INDEX.get(value);
        }

        @Override
        public String getValue(Integer code) {
            return REVERSE.get(code);
        }
    }
}
