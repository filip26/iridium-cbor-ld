package com.apicatalog.cborld.document;

import java.util.Map;

import com.apicatalog.cborld.dictionary.Dictionary;

public interface DocumentDictionary {

    int code();

    Dictionary contexts();

    Map<String, Dictionary> types();
}
