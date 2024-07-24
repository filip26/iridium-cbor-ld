package com.apicatalog.cborld.barcode;

import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.dictionary.CustomDictionary;
import com.apicatalog.cborld.dictionary.Dictionary;


public class BarcodesDictionary extends CustomDictionary {
    
    static final Map<String, Dictionary> types = new HashMap<>();
    
    static {
        types.put("https://w3id.org/security#cryptosuiteString", BarcodesTypeDictionary.INSTANCE);
    }
    
    public BarcodesDictionary() {
        super(BarcodesContextDictionary.INSTANCE, types);
    }
}
