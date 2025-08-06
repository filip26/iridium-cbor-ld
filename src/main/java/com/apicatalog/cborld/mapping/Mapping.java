package com.apicatalog.cborld.mapping;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.registry.DocumentDictionary;

public interface Mapping {
    // static dictionary based
    DocumentDictionary dictionary();

    // dynamic
    Dictionary termMap();

    TypeMap typeMap();
}
