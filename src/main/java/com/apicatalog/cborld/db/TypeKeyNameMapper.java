package com.apicatalog.cborld.db;

import java.util.Collection;

public interface TypeKeyNameMapper {

    void beginMap(String key);
    void typeKeyName(String type);
    void end();
    
    boolean isTypeKey(String term, Collection<String> termPath);
}
