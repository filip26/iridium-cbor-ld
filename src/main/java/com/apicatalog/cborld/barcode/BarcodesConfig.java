package com.apicatalog.cborld.barcode;

import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.document.CompressedDocument;
import com.apicatalog.cborld.document.DocumentDictionary;

public class BarcodesConfig extends DefaultConfig {

    public static final BarcodesConfig INSTANCE = new BarcodesConfig();
    
    static final Map<Integer, DocumentDictionary> DICTIONARIES;

    static {
        DICTIONARIES = new HashMap<>();
        DICTIONARIES.put(CompressedDocument.DICTIONARY.code(), CompressedDocument.DICTIONARY);
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
