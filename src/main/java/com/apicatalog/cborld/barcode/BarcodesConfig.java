package com.apicatalog.cborld.barcode;

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

    public static byte DICTIONARY_CODE = 100;

    static {
        DICTIONARY = DocumentDictionaryBuilder.create(DICTIONARY_CODE)
                .context(32768, "https://www.w3.org/ns/credentials/v2")
                .context(32769, "https://w3id.org/vc-barcodes/v1")
                .context(32770, "https://w3id.org/utopia/v2")
                .type("https://w3id.org/security#cryptosuiteString",
                        DictionaryBuilder.create()
                                .set(1, "ecdsa-rdfc-2019")
                                .set(2, "ecdsa-sd-2023")
                                .set(3, "eddsa-rdfc-2022")
                                .set(4, "ecdsa-xi-2023"))
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
