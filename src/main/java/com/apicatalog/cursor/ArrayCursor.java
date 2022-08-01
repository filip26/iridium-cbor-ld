package com.apicatalog.cursor;

import java.util.function.Function;
import java.util.function.Predicate;

public interface ArrayCursor extends StructureCursor, Iterable<ArrayItemCursor> {

    ArrayItemCursor item(int index);
    
    default boolean is(int index, Predicate<ValueCursor> predicate) {

        ArrayItemCursor item = item(index); 
        
        boolean result = predicate.test(item);
        
        item.parent();
        
        return result;
    }
    
    default <T> T value(int index, Function<ValueCursor, T> getter) {
        
        ArrayItemCursor item = item(index); 
        
        T result = getter.apply(item); 
        
        item.parent();
        
        return result;        
    }
    
    default boolean isNull(int index) {
        return is(index, ValueCursor::isNull);
    }

    default boolean isString(int index) {
        return is(index, ValueCursor::isString);
    }
    
    default boolean isBoolean(int index) {
        return is(index, ValueCursor::isBoolean);
    }
    
    default boolean isNumber(int index) {
        return is(index, ValueCursor::isNumber);
    }

    default boolean isPrimitive(int index) {
        return isString(index) || isBoolean(index) || isNumber(index);
    }

    default boolean isArray(int index) {
        return is(index, ValueCursor::isArray);
    }
    
    default boolean isNonEmptyArray(int index) {
        return is(index, ValueCursor::isNonEmptyArray);
    }

    default boolean isMap(int index) {
        return is(index, ValueCursor::isMap);
    }
    
    default boolean isNonEmptyMap(int index) {
        return is(index, ValueCursor::isNonEmptyMap);
    }

    default boolean isStructure(int index) {
        return isArray(index) || isMap(index);
    }

    default Boolean booleanValue(int index) {
        return value(index, ValueCursor::booleanValue);
    }

    default Integer integerValue(int index) {
        return value(index, ValueCursor::integerValue);        
    }
    
    default Long longValue(int index) {
        return value(index, ValueCursor::longValue);
    }

    default String stringValue(int index) {
        return value(index, ValueCursor::stringValue);
    }
//
//    ArrayCursor array(int index);
//    MapCursor object(int index);
//    ValueCursor value(int index);
}
