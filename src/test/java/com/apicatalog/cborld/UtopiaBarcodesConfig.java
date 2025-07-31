package com.apicatalog.cborld;

import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.cborld.registry.DocumentDictionaryBuilder;
import com.apicatalog.cborld.registry.LegacyDictionary;

public class UtopiaBarcodesConfig extends BarcodesConfig {

    public static final UtopiaBarcodesConfig INSTANCE;

    public static final DocumentDictionary DICTIONARY;

    static final Map<Integer, DocumentDictionary> DICTIONARIES;

    static {
        DICTIONARY = DocumentDictionaryBuilder.of(BarcodesConfig.DICTIONARY)
                .context("https://w3id.org/utopia/v2", 32770)
                .build();

        DICTIONARIES = new HashMap<>();
        DICTIONARIES.put(LegacyDictionary.DICTIONARY.code(), LegacyDictionary.DICTIONARY);
        DICTIONARIES.put(DICTIONARY.code(), DICTIONARY);

        INSTANCE = new UtopiaBarcodesConfig();
    }

    @Override
    public DocumentDictionary dictionary() {
        return UtopiaBarcodesConfig.DICTIONARY;
    }

    @Override
    public Map<Integer, DocumentDictionary> registry() {
        return UtopiaBarcodesConfig.DICTIONARIES;
    }
}
