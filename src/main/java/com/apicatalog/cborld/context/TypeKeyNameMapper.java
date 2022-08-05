package com.apicatalog.cborld.context;

public interface TypeKeyNameMapper {

    void beginMap(String key);
    void typeKeyName(String type);
    void end();
}
