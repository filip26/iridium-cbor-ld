package com.apicatalog.cborld.mapping;

public interface TypeKeyNameMapper {

    void beginMap(String key);
    void typeKeyName(String type);
    void end();
    
    boolean isTypeKey(String term);
}
