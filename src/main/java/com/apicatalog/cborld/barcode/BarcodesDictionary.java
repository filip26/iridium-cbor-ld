package com.apicatalog.cborld.barcode;

import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.dictionary.DictionaryBuilder;
import com.apicatalog.cborld.dictionary.DocumentDictionary;


public class BarcodesDictionary extends DocumentDictionary {

    public static final BarcodesDictionary INSTANCE;

    public static byte CODE = 100;
    
    final static Dictionary CONTEXTS;
    
    final static Dictionary CRYPTOSUITE;

    static final Map<String, Dictionary> TYPES;

    static {
        CONTEXTS = DictionaryBuilder.create()
                .set(32768, "https://www.w3.org/ns/credentials/v2")
                .set(32769, "https://w3id.org/vc-barcodes/v1")
                .set(32770, "https://w3id.org/utopia/v2")
                .build();
        
        CRYPTOSUITE = DictionaryBuilder.create()
                .set(1, "ecdsa-rdfc-2019")
                .set(2, "ecdsa-sd-2023")
                .set(3, "eddsa-rdfc-2022")
                .set(4, "ecdsa-xi-2023")
                .build();

        TYPES = new HashMap<>();
        TYPES.put("https://w3id.org/security#cryptosuiteString", CRYPTOSUITE);
        INSTANCE = new BarcodesDictionary(CONTEXTS, TYPES);
    }
    
    protected BarcodesDictionary(Dictionary contexts, Map<String, Dictionary> types) {
        super(CODE, contexts, types);
    }
}
