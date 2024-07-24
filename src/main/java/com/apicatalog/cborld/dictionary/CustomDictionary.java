package com.apicatalog.cborld.dictionary;

import java.util.Map;

public class CustomDictionary {

    protected final Dictionary contexts;
    protected final Map<String, Dictionary> types;

    public CustomDictionary(Dictionary context, Map<String, Dictionary> types) {
        this.contexts = context;
        this.types = types;
    }

    public Dictionary contexts() {
        return contexts;
    }
    
    public Map<String, Dictionary> types() {
        return types;
    }

}
