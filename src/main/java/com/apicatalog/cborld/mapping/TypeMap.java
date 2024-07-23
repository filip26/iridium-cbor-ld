package com.apicatalog.cborld.mapping;

import java.util.Collection;

public interface TypeMap {

    Collection<String> getType(String term);

    TypeMap getMapping(String term);
}
