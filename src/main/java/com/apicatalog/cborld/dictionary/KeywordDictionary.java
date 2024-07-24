package com.apicatalog.cborld.dictionary;

import java.util.HashMap;
import java.util.Map;

class KeywordDictionary implements Dictionary {

    public static final byte CONTEXT_CODE = 0;

    public static final byte CUSTOM_OFFSET = 0x64; // 100 decimal

    protected static final Map<String, Integer> TERM_TO_CODE = new HashMap<>();
    protected static final Map<Integer, String> CODE_TO_TERM = new HashMap<>();

    static {
        add("@context", (int) CONTEXT_CODE);
        add("@type", 2);
        add("@id", 4);
        add("@value", 6);

        add("@direction", 8);
        add("@graph", 10);
        add("@included", 12);
        add("@index", 14);
        add("@json", 16);
        add("@language", 18);
        add("@list", 20);
        add("@nest", 22);
        add("@reverse", 24);

        add("@base", 26);
        add("@container", 28);
        add("@default", 30);
        add("@embed", 32);
        add("@explicit", 34);
        add("@none", 36);
        add("@omitDefault", 38);
        add("@prefix", 40);
        add("@preserve", 42);
        add("@protected", 44);
        add("@requireAll", 46);
        add("@set", 48);
        add("@version", 50);
        add("@vocab", 52);
    }

    static final void add(String term, Integer code) {
        TERM_TO_CODE.put(term, code);
        CODE_TO_TERM.put(code, term);
    }

    @Override
    public Integer getCode(String term) {
        return TERM_TO_CODE.get(term);
    }

    @Override
    public String getValue(Integer code) {
        return CODE_TO_TERM.get(code);
    }
}
