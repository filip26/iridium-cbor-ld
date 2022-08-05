package com.apicatalog.cborld.context;

public interface TypeMapper {

    void beginMap(String key);
    void typeKeyName(String type);
    void end();
}
