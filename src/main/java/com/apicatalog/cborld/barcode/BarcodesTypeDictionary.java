package com.apicatalog.cborld.barcode;

import java.math.BigInteger;
import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.cborld.dictionary.Dictionary;


public class BarcodesTypeDictionary implements Dictionary {

    protected final static Map<String, BigInteger> INDEX = new LinkedHashMap<>();
    protected final static Map<BigInteger, String> REVERSE = new LinkedHashMap<>();

    static {
        add(1, "ecdsa-rdfc-2019");
        add(2, "ecdsa-sd-2023");
        add(3, "eddsa-rdfc-2022");
        add(4, "ecdsa-xi-2023");
    }

    protected static void add(Integer code, String value) {
        INDEX.put(value, BigInteger.valueOf(code));
        REVERSE.put(BigInteger.valueOf(code), value);
    }
    
    @Override
    public BigInteger getCode(String value) {
        return INDEX.get(value);
    }

    @Override
    public String getValue(BigInteger code) {
        return REVERSE.get(code);
    }
}
