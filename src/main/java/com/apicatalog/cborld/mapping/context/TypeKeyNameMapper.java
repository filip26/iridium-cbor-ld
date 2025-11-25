package com.apicatalog.cborld.mapping.context;

interface TypeKeyNameMapper  {

    void beginMap(String key);

    void typeKey(String key);

    void endMap();

    boolean isTypeKey(String term);
}
