package com.apicatalog.cborld.document;

import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.dictionary.DictionaryBuilder;

public class DocumentDictionaryBuilder {

    protected final int code;
    protected final DictionaryBuilder contexts;
    protected final Map<String, DictionaryBuilder> types;

    protected DocumentDictionaryBuilder(int code, DictionaryBuilder contexts, Map<String, DictionaryBuilder> types) {
        this.code = code;
        this.contexts = contexts;
        this.types = types;
    }
    
    public static DocumentDictionaryBuilder create(int code) {
        return new DocumentDictionaryBuilder(code, DictionaryBuilder.create(), new HashMap<>());
    }

    public DocumentDictionary build() {
        return new DocumentDictionaryImpl(
                code,
                contexts.build(),
                types.entrySet()
                        .stream()
                        .map(e -> new AbstractMap.SimpleEntry<>(e.getKey(), e.getValue().build()))
                        .collect(Collectors.toMap(
                                Map.Entry::getKey,
                                Map.Entry::getValue)));
    }

    public DocumentDictionaryBuilder context(int code, String value) {
        contexts.set(code, value);
        return this;
    }

    public DocumentDictionaryBuilder context(Dictionary dictionary) {
        contexts.merge(dictionary);
        return this;
    }

    public DocumentDictionaryBuilder type(String name, int code, String value) {
        DictionaryBuilder dictionary = types.get(name);
        if (dictionary == null) {
            dictionary = DictionaryBuilder.create();
            types.put(name, dictionary);
        }
        dictionary.set(code, value);
        return this;
    }

    public DocumentDictionaryBuilder type(String name, DictionaryBuilder builder) {
        types.put(name, builder);
        return this;
    }

    class DocumentDictionaryImpl implements DocumentDictionary {

        protected final int code;
        protected final Dictionary contexts;
        protected final Map<String, Dictionary> types;

        public DocumentDictionaryImpl(final int code, Dictionary contexts, Map<String, Dictionary> types) {
            this.code = code;
            this.contexts = contexts;
            this.types = types;
        }

        public int code() {
            return code;
        }

        public Dictionary contexts() {
            return contexts;
        }

        public Map<String, Dictionary> types() {
            return types;
        }
    }
}
