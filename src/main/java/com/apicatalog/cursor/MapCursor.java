package com.apicatalog.cursor;

import java.util.Collection;

public interface MapCursor extends StructureCursor {

    public interface Key {
     
        boolean isInteger();
        boolean isString();
        
        String stringValue();
        int intValue();
    }
    
    Collection<String> keys();

    boolean has(String key);
    boolean has(int key);
    
    default boolean has(Key key) {
        return key.isString() ? has(key.stringValue()) : has(key.intValue());
    }

    boolean isNull(String key);
    boolean isNull(int key);
    
    boolean isString(String key);
    boolean isString(int key);
    
    boolean isBoolean(String key);
    boolean isBoolean(int key);
    
    boolean isNumber(String key);
    boolean isNumber(int key);

    default boolean isPrimitive(String key) {
        return isString(key) || isBoolean(key) || isNumber(key);
    }

    default boolean isPrimitive(int key) {
        return isString(key) || isBoolean(key) || isNumber(key);
    }

    boolean isArray(String key);
    boolean isArray(int key);
    
    boolean isNonEmptyArray(String key);
    boolean isNonEmptyArray(int key);

    boolean isObject(String key);
    boolean isObject(int key);
    
    boolean isNonEmptyObject(String key);
    boolean isNonEmptyObject(int key);

    default boolean isStructure(String key) {
        return isArray(key) || isObject(key);
    }
    
    default boolean isStructure(int key) {
        return isArray(key) || isObject(key);
    }

    Boolean booleanValue(String key);
    Boolean booleanValue(int key);

    Integer integerValue(String key);
    Integer integerValue(int key);

    Long longValue(String key);
    Long longValue(int key);

    String stringValue(String key);
    String stringValue(int key);

    ArrayCursor array(String key);

    MapCursor object(String key);

    ValueCursor value(String key);

    MapCursor clone();
}
