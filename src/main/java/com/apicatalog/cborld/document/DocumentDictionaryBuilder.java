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
    protected final DictionaryBuilder uris;

    protected DocumentDictionaryBuilder(int code, DictionaryBuilder contexts, Map<String, DictionaryBuilder> types, final DictionaryBuilder uris) {
        this.code = code;
        this.contexts = contexts;
        this.types = types;
        this.uris = uris;
    }

    public static DocumentDictionaryBuilder create(int code) {
        return new DocumentDictionaryBuilder(code, DictionaryBuilder.create(), new HashMap<>(), DictionaryBuilder.create());
    }

    public static DocumentDictionaryBuilder of(DocumentDictionary dictionary) {
        return new DocumentDictionaryBuilder(
                dictionary.code(),
                DictionaryBuilder.of(dictionary.contexts()),
                dictionary.types()
                        .entrySet()
                        .stream()
                        .map(e -> new AbstractMap.SimpleEntry<>(e.getKey(), DictionaryBuilder.of(e.getValue())))
                        .collect(Collectors.toMap(
                                Map.Entry::getKey,
                                Map.Entry::getValue)),
                DictionaryBuilder.of(dictionary.uris()));
    }

    public DocumentDictionary build() {
        return new DocumentDictionaryImpl(
                code,
                contexts.build(),
                types.entrySet()
                        .stream()
                        .map(e -> new AbstractMap.SimpleEntry<>(e.getKey(), e.getValue().build()))
                        .collect(Collectors.toUnmodifiableMap(
                                Map.Entry::getKey,
                                Map.Entry::getValue)),
                uris.build());
    }

    public DocumentDictionaryBuilder context(String value, int code) {
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

    public DocumentDictionaryBuilder uri(String value, int code) {
        uris.set(code, value);
        return this;
    }

    public DocumentDictionaryBuilder uri(Dictionary dictionary) {
        uris.merge(dictionary);
        return this;
    }

    
    class DocumentDictionaryImpl implements DocumentDictionary {

        protected final int code;
        protected final Dictionary contexts;
        protected final Map<String, Dictionary> types;
        protected final Dictionary uris;

        public DocumentDictionaryImpl(final int code, Dictionary contexts, Map<String, Dictionary> types, Dictionary uris) {
            this.code = code;
            this.contexts = contexts;
            this.types = types;
            this.uris = uris;
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

        @Override
        public Dictionary uris() {
            return uris;
        }
    }
}
