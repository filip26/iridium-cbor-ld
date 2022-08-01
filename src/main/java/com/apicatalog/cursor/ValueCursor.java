package com.apicatalog.cursor;

import java.util.Optional;

public interface ValueCursor {

    Optional<ValueCursor> parent();

    boolean isNull();

    boolean isString();
    String stringValue();
    
    boolean isBoolean();
    Boolean booleanValue();

    boolean isNumber();
    Integer integerValue();
    Long longValue();

    default boolean isPrimitive() {
        return isString() || isBoolean() || isNumber();
    }

    boolean isArray();
    default boolean isNonEmptyArray() {
        return isArray() && !asArray().isEmpty();
    }
    
    ArrayCursor asArray();

    boolean isMap();
    default boolean isNonEmptyMap() {
        return isMap() && !asMap().isEmpty();
    }
    
    MapCursor asMap();

    default boolean isStructure() {
        return isArray() || isMap();
    }
    
//    ValueCursor clone();
    
    boolean isArrayItem();
    ArrayItemCursor asArrayItem();
    
    boolean isMapEntry();
    MapEntryCursor asMapEntry();
}
