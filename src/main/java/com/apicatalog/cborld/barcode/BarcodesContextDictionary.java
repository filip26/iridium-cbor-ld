package com.apicatalog.cborld.barcode;

import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.cborld.dictionary.Dictionary;


class BarcodesContextDictionary implements Dictionary {

    protected final static Map<String, Integer> INDEX = new LinkedHashMap<>();
    protected final static Map<Integer, String> REVERSE = new LinkedHashMap<>();

    final static BarcodesContextDictionary INSTANCE = new BarcodesContextDictionary();
    
    static {
        add(32768, "https://www.w3.org/ns/credentials/v2");
        add(32769, "https://w3id.org/vc-barcodes/v1");
        add(32770, "https://w3id.org/utopia/v2");
    }

    protected static void add(Integer code, String value) {
        INDEX.put(value, code);
        REVERSE.put(code, value);
    }
    
    @Override
    public Integer getCode(String value) {
        return INDEX.get(value);
    }

    @Override
    public String getValue(Integer code) {
        return REVERSE.get(code);
    }
}
