package com.apicatalog.cborld.dictionary;

import java.math.BigInteger;
import java.util.LinkedHashMap;
import java.util.Map;


public class BarcodesDictionary implements Dictionary {

    protected final static Map<String, BigInteger> INDEX = new LinkedHashMap<>();
    protected final static Map<BigInteger, String> REVERSE = new LinkedHashMap<>();

    static {
        add(32768, "https://www.w3.org/ns/credentials/v2");
        add(32769, "https://w3id.org/vc-barcodes/v1");
        add(32770, "https://w3id.org/utopia/v2");
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
