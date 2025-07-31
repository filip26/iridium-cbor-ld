package com.apicatalog.cborld.registry;

import java.util.Map;

import com.apicatalog.cborld.dictionary.Dictionary;

public interface DocumentDictionary {

    int code();

    Dictionary contexts();

    Map<String, Dictionary> types();

    Dictionary uris();
}
