package com.apicatalog.cborld.registry;

import java.util.Collections;
import java.util.Map;

import com.apicatalog.cborld.dictionary.Dictionary;

public record DefaultDocumentDictionary(int code) implements DocumentDictionary {

    @Override
    public Dictionary contexts() {
        return null;
    }

    @Override
    public Map<String, Dictionary> types() {
        return Collections.emptyMap();
    }

    @Override
    public Dictionary uris() {
        return null;
    }
}
