package com.apicatalog.cborld.registry;

import java.util.Collections;
import java.util.Map;

import com.apicatalog.cborld.mapping.TermMap;

// use builder
@Deprecated
public record DefaultDocumentDictionary(int code) implements DocumentDictionary {

    @Override
    public TermMap contexts() {
        return null;
    }

    @Override
    public Map<String, TermMap> types() {
        return Collections.emptyMap();
    }

    @Override
    public TermMap uris() {
        return null;
    }
}
