package com.apicatalog.cborld.mapping;

public interface TypeKeyNameMapper  {

    void beginMap(String key);

    void typeKeyName(String type);

    void endMap();

    boolean isTypeKey(String term);
}
