package com.apicatalog.cborld;

import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.dictionary.DictionaryBuilder;
import com.apicatalog.cborld.document.CompressedDocument;
import com.apicatalog.cborld.document.DocumentDictionary;
import com.apicatalog.cborld.document.DocumentDictionaryBuilder;

public class BarcodesConfig extends DefaultConfig {

    public static final BarcodesConfig INSTANCE = new BarcodesConfig();

    public static final DocumentDictionary DICTIONARY;

    static final Map<Integer, DocumentDictionary> DICTIONARIES;

    public static final byte DICTIONARY_CODE = 100;

    static {
        DICTIONARY = DocumentDictionaryBuilder.create(DICTIONARY_CODE)
                .context("https://www.w3.org/ns/credentials/v2", 32768)
                .context("https://w3id.org/vc-barcodes/v1", 32769)
                .type("https://w3id.org/security#cryptosuiteString",
                        DictionaryBuilder.create()
                                .set("ecdsa-rdfc-2019", 1)
                                .set("ecdsa-sd-2023", 2)
                                .set("eddsa-rdfc-2022", 3)
                                .set("ecdsa-xi-2023", 4))
                .build();

        DICTIONARIES = new HashMap<>();
        DICTIONARIES.put(CompressedDocument.DICTIONARY.code(), CompressedDocument.DICTIONARY);
        DICTIONARIES.put(DICTIONARY.code(), DICTIONARY);
    }

    @Override
    public DocumentDictionary dictionary() {
        return DICTIONARY;
    }

    @Override
    public Map<Integer, DocumentDictionary> dictionaries() {
        return DICTIONARIES;
    }
}
