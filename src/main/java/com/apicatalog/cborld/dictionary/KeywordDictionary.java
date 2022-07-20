package com.apicatalog.cborld.dictionary;

import java.util.HashMap;
import java.util.Map;

public class KeywordDictionary implements Dictionary {

    public static final byte CONTEXT_CODE = 0;
    
    private static final byte OFFSET = 0x64;	// 100 decimal
        
    private static final Map<String, Integer> TERM_TO_CODE = new HashMap<>();
    private static final Map<Integer, String> CODE_TO_TERM = new HashMap<>();

    static {
	add("@context", (int)CONTEXT_CODE);
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
    }

    static final void add(String term, Integer code) {
	TERM_TO_CODE.put(term, code);
	CODE_TO_TERM.put(code, term);
    }
    
    @Override
    public byte[] getCode(String term) {
	Integer code = TERM_TO_CODE.get(term);
	
	if (code != null) {
	    return new byte[] { (byte)(int)code };
	}

	return null;
    }

    @Override
    public String getTerm(byte[] code) {
	return CODE_TO_TERM.get((int)code[0]);
    }
}
