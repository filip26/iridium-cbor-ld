package com.apicatalog.cborld.barcode;

import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.dictionary.ContextDictionary;
import com.apicatalog.cborld.dictionary.CustomDictionary;

public class BarcodesConfig extends DefaultConfig {

    public static final BarcodesConfig INSTANCE = new BarcodesConfig();
    
    static final Map<Integer, CustomDictionary> DICTIONARIES;

    static final CustomDictionary DICTIONARY = new CustomDictionary(0x01, ContextDictionary.INSTANCE, null);

    static {
        DICTIONARIES = new HashMap<>();
        DICTIONARIES.put(0x01, DICTIONARY);
        DICTIONARIES.put(BarcodesDictionary.INSTANCE.code(), BarcodesDictionary.INSTANCE);
    }

    @Override
    public CustomDictionary dictionary() {
        return BarcodesDictionary.INSTANCE;
    }
    
    @Override
    public Map<Integer, CustomDictionary> dictionaries() {
        return DICTIONARIES;
    }
}
