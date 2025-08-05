package com.apicatalog.cborld.registry;

import java.util.Map;

import com.apicatalog.cborld.dictionary.Dictionary;

public class DefaultDocumentDictionary implements DocumentDictionary {

    protected final int code;

    public DefaultDocumentDictionary(int code) {
        this.code = code;
    }

    @Override
    public int code() {
        return code;
    }

    @Override
    public Dictionary contexts() {
        return null;
    }

    @Override
    public Map<String, Dictionary> types() {
        return null;
    }

    @Override
    public Dictionary uris() {
        return null;
    }
}
