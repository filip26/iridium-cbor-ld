package com.apicatalog.cborld.dictionary;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

public class DictionaryBuilder {

    protected final Map<Integer, String> reverse;

    protected DictionaryBuilder() {
        this.reverse = new LinkedHashMap<>();
    }

    protected DictionaryBuilder(BidirectionalDictionary dictionary) {
        this.reverse = new LinkedHashMap<>(dictionary.reverse());
    }

    public static DictionaryBuilder create() {
        return new DictionaryBuilder();
    }

    public static DictionaryBuilder of(Dictionary dictionary) {
        if (dictionary instanceof BidirectionalDictionary) {
            return new DictionaryBuilder((BidirectionalDictionary) dictionary);
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
        return new BidirectionalDictionary(
                reverse.entrySet()
                        .stream()
                        .collect(Collectors.toUnmodifiableMap(Entry::getValue, Entry::getKey)),
                Collections.unmodifiableMap(reverse));
    }
}
