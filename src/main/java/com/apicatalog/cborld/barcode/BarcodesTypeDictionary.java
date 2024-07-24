package com.apicatalog.cborld.barcode;

import java.math.BigInteger;
import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.cborld.dictionary.Dictionary;


class BarcodesTypeDictionary implements Dictionary {

    protected final static Map<String, Integer> INDEX = new LinkedHashMap<>();
    protected final static Map<Integer, String> REVERSE = new LinkedHashMap<>();

    final static BarcodesTypeDictionary INSTANCE = new BarcodesTypeDictionary();
    
    static {
        add(1, "ecdsa-rdfc-2019");
        add(2, "ecdsa-sd-2023");
        add(3, "eddsa-rdfc-2022");
        add(4, "ecdsa-xi-2023");
    }

    protected static void add(Integer code, String value) {
        INDEX.put(value, code);
        REVERSE.put(code, value);
    }
    
    @Override
    public BigInteger getCode(String value) {
        return INDEX.containsKey(value) ? BigInteger.valueOf(INDEX.get(value)) : null;
    }

    @Override
    public String getValue(BigInteger code) {
        return REVERSE.get(code.intValueExact());
    }
}
