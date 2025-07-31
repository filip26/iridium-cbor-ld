package com.apicatalog.cborld;

import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.document.CompressedDocument;
import com.apicatalog.cborld.document.DocumentDictionary;
import com.apicatalog.cborld.document.DocumentDictionaryBuilder;

public class UtopiaBarcodesConfig extends BarcodesConfig {

    public static final UtopiaBarcodesConfig INSTANCE;

    public static final DocumentDictionary DICTIONARY;

    static final Map<Integer, DocumentDictionary> DICTIONARIES;

    static {
        DICTIONARY = DocumentDictionaryBuilder.of(BarcodesConfig.DICTIONARY)
                .context("https://w3id.org/utopia/v2", 32770)
                .build();

        DICTIONARIES = new HashMap<>();
        DICTIONARIES.put(CompressedDocument.DICTIONARY.code(), CompressedDocument.DICTIONARY);
        DICTIONARIES.put(DICTIONARY.code(), DICTIONARY);

        INSTANCE = new UtopiaBarcodesConfig();
    }

    @Override
    public DocumentDictionary dictionary() {
        return UtopiaBarcodesConfig.DICTIONARY;
    }

    @Override
    public Map<Integer, DocumentDictionary> dictionaries() {
        return UtopiaBarcodesConfig.DICTIONARIES;
    }
}
