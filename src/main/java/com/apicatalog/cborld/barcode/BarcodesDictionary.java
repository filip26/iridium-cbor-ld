package com.apicatalog.cborld.barcode;

import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.dictionary.DocumentDictionary;
import com.apicatalog.cborld.dictionary.Dictionary;


public class BarcodesDictionary extends DocumentDictionary {

    public static final BarcodesDictionary INSTANCE;
    static final Map<String, Dictionary> TYPES;
    
    static {
        TYPES = new HashMap<>();
        TYPES.put("https://w3id.org/security#cryptosuiteString", BarcodesTypeDictionary.INSTANCE);
        INSTANCE = new BarcodesDictionary(TYPES);
    }
    
    protected BarcodesDictionary(Map<String, Dictionary> types) {
        super(100, BarcodesContextDictionary.INSTANCE, types);
    }
}
