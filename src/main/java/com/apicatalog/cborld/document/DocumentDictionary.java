package com.apicatalog.cborld.document;

import java.util.Map;

import com.apicatalog.cborld.dictionary.Dictionary;

public class DocumentDictionary {

    protected final int code;
    protected final Dictionary contexts;
    protected final Map<String, Dictionary> types;

    public DocumentDictionary(final int code, Dictionary context, Map<String, Dictionary> types) {
        this.code = code;
        this.contexts = context;
        this.types = types;
    }
    
    public int code() {
        return code;
    }

    public Dictionary contexts() {
        return contexts;
    }
    
    public Map<String, Dictionary> types() {
        return types;
    }

}
