package com.apicatalog.cborld.barcode;

import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.dictionary.ContextDictionary;
import com.apicatalog.cborld.dictionary.DocumentDictionary;

public class BarcodesConfig extends DefaultConfig {

    public static final BarcodesConfig INSTANCE = new BarcodesConfig();
    
    static final Map<Integer, DocumentDictionary> DICTIONARIES;

    static final DocumentDictionary DICTIONARY = new DocumentDictionary(0x01, ContextDictionary.INSTANCE, null);

    static {
        DICTIONARIES = new HashMap<>();
        DICTIONARIES.put(0x01, DICTIONARY);
        DICTIONARIES.put(BarcodesDictionary.INSTANCE.code(), BarcodesDictionary.INSTANCE);
    }

    @Override
    public DocumentDictionary dictionary() {
        return BarcodesDictionary.INSTANCE;
    }
    
    @Override
    public Map<Integer, DocumentDictionary> dictionaries() {
        return DICTIONARIES;
    }
}
