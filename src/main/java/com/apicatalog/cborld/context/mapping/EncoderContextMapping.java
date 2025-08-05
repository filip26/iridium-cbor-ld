package com.apicatalog.cborld.context.mapping;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.cborld.registry.DocumentDictionary;

record EncoderContextMapping(
        DocumentDictionary dictionary,
        Dictionary termMap,
        TypeMap typeMap) implements Mapping {
}
