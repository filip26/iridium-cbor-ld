package com.apicatalog.cborld.mapping;

public interface TypeKeyNameMapper  {

    void beginMap(String key);

    void typeKey(String key);

    void endMap();

    boolean isTypeKey(String term);
}
