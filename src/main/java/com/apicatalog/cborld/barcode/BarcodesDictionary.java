package com.apicatalog.cborld.barcode;

import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.dictionary.CustomDictionary;
import com.apicatalog.cborld.dictionary.Dictionary;


public class BarcodesDictionary extends CustomDictionary {
    
    public static final BarcodesDictionary INSTANCE = new BarcodesDictionary();
    
    static final Map<String, Dictionary> TYPES = new HashMap<>();
    
    static {
        TYPES.put("https://w3id.org/security#cryptosuiteString", BarcodesTypeDictionary.INSTANCE);
    }
    
    protected BarcodesDictionary() {
        super(100, BarcodesContextDictionary.INSTANCE, TYPES);
    }
}
