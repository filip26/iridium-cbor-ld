package com.apicatalog.cborld.mapping;

import com.apicatalog.cborld.dictionary.Dictionary;

public interface Mapping {

    Dictionary context();

    Dictionary type(String type);

    Dictionary terms();

    TypeMap typeMap();
}
