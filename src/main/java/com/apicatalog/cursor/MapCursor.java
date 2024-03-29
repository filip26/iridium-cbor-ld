package com.apicatalog.cursor;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.function.Function;
import java.util.function.Predicate;

public interface MapCursor extends StructureCursor, Iterable<MapEntryCursor> {

    Collection<String> keys();

    MapEntryCursor entry(String key);
    
    default MapEntryCursor entry() {
        if (isEmpty()) {
            throw new IndexOutOfBoundsException();
        }
        return entry(keys().iterator().next());
    }
    
    default boolean is(String key, Predicate<ValueCursor> predicate) {

        final MapEntryCursor entry = entry(key); 
        
        boolean result = predicate.test(entry);
        
        entry.parent();
        
        return result;
    }
    
    default <T> T value(String key, Function<ValueCursor, T> getter) {
        
        final MapEntryCursor entry = entry(key); 
        
        T result = getter.apply(entry); 
        
        entry.parent();
        
        return result;        
    }

    boolean contains(String key);

    default boolean isNull(String key) {
        return is(key, ValueCursor::isNull);
    }

    default boolean isString(String key) {
        return is(key, ValueCursor::isString);
    }

    default boolean isBoolean(String key) {
        return is(key, ValueCursor::isBoolean);
    }
    
    default boolean isInteger(String key) {
        return is(key, ValueCursor::isInteger);
    }

    default boolean isDecimal(String key) {
        return is(key, ValueCursor::isDecimal);
    }

    default boolean isNumber(String key) {
        return isInteger(key) || isDecimal(key);
    }

    default boolean isScalar(String key) {
        return isString(key) || isBoolean(key) || isNumber(key);
    }

    default boolean isArray(String key) {
        return is(key, ValueCursor::isArray);
    }
    
    default boolean isNonEmptyArray(String key) {
        return is(key, ValueCursor::isNonEmptyArray);
    }

    default boolean isMap(String key) {
        return is(key, ValueCursor::isMap);
    }

    default boolean isNonEmptyMap(String key) {
        return is(key, ValueCursor::isNonEmptyMap);
    }

    default boolean isStructure(String key) {
        return isArray(key) || isMap(key);
    }
    
    default Boolean booleanValue(String key) {
        return value(key, ValueCursor::booleanValue);
    }

    default BigInteger integerValue(String key) {
        return value(key, ValueCursor::integerValue);
    }

    default BigDecimal decimalValue(String key) {
        return value(key, ValueCursor::decimalValue);
    }

    default String stringValue(String key) {
        return value(key, ValueCursor::stringValue);
    }
}
