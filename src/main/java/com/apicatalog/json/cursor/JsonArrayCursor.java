package com.apicatalog.json.cursor;

public interface JsonArrayCursor extends JsonValueCursor {

    boolean isNull(int index);
    
    boolean isString(int index);
    boolean isBoolean(int index);
    boolean isNumber(int index);
    
    default boolean isPrimitive(int index) {
	return isString(index) || isBoolean(index) || isNumber(index);
    }
    
    boolean isArray(int index);
    boolean isNonEmptyArray(int index);
    
    boolean isObject(int index);
    boolean isNonEmptyObject(int index);
    
    default boolean isStructure(int index) {
	return isArray(index) || isObject(index);
    }
    
    Boolean booleanValue(int index);

    Integer integerValue(int index);
    Long longValue(int index);

    String stringValue(int index);

    JsonArrayCursor array(int index);
    JsonObjectCursor object(int index);
    JsonValueCursor value(int index);
}
