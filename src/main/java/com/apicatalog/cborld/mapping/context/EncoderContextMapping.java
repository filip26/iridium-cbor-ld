package com.apicatalog.cborld.mapping.context;

import com.apicatalog.cborld.mapping.TermMap;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.cborld.registry.DocumentDictionary;

record EncoderContextMapping(
        DocumentDictionary dictionary,
        TermMap termMap,
        TypeMap typeMap) implements Mapping {
}
