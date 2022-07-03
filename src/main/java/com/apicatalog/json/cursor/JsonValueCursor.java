package com.apicatalog.json.cursor;

import java.util.Optional;

public interface JsonValueCursor {

    Optional<JsonValueCursor> parent();

    boolean isNull();

    boolean isString();
    boolean isBoolean();
    boolean isNumber();
    
    default boolean isPrimitive() {
	return isString() || isBoolean() || isNumber();
    }
    
    boolean isArray();
    boolean isNonEmptyArray();

    boolean isObject();
    boolean isNonEmptyObject();
    
    default boolean isStructure() {
	return isArray() || isObject();
    }
    
    Boolean booleanValue();

    Integer integerValue();
    Long longValue();

    String stringValue();

    JsonObjectCursor asObject();
    JsonArrayCursor asArray();
    
    int size();
}
