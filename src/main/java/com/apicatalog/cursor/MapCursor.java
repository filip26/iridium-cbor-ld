package com.apicatalog.cursor;

import java.math.BigInteger;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;

public interface MapCursor extends StructureCursor, Iterable<MapEntryCursor> {

//    Collection<String> keys();

    MapEntryCursor entry(String key);

    MapEntryCursor entry(BigInteger key);
    
    default Iterable<MapEntryCursor> entries() {
        return entries(false);
    }

    Iterable<MapEntryCursor> entries(boolean sorted);
    
//    default MapEntryCursor entry() {
//        if (isEmpty()) {
//            throw new IndexOutOfBoundsException();
//        }
//        return entry(keys().iterator().next());
//    }
    
    default boolean is(String key, Predicate<DataCursor> predicate) {

        final MapEntryCursor entry = entry(key); 
        
        boolean result = predicate.test(entry);
        
        entry.parent();
        
        return result;
    }
    
    default <T> T value(String key, Function<DataCursor, T> getter) {
        
        final MapEntryCursor entry = entry(key); 
        
        T result = getter.apply(entry); 
        
        entry.parent();
        
        return result;        
    }

    boolean contains(String key);

    default boolean isNull(String key) {
        return is(key, DataCursor::isNull);
    }

    default boolean isString(String key) {
        return is(key, DataCursor::isString);
    }

    default boolean isBoolean(String key) {
        return is(key, DataCursor::isBoolean);
    }
    
    default boolean isNumber(String key) {
        return is(key, DataCursor::isNumber);
    }

    default boolean isScalar(String key) {
        return isString(key) || isBoolean(key) || isNumber(key);
    }

    default boolean isArray(String key) {
        return is(key, DataCursor::isArray);
    }
    
    default boolean isNonEmptyArray(String key) {
        return is(key, DataCursor::isNonEmptyArray);
    }

    default boolean isMap(String key) {
        return is(key, DataCursor::isMap);
    }

    default boolean isNonEmptyMap(String key) {
        return is(key, DataCursor::isNonEmptyMap);
    }

    default boolean isStructure(String key) {
        return isArray(key) || isMap(key);
    }
    
    default Boolean booleanValue(String key) {
        return value(key, DataCursor::booleanValue);
    }

    default Integer integerValue(String key) {
        return value(key, DataCursor::integerValue);
    }

    default Long longValue(String key) {
        return value(key, DataCursor::longValue);
    }

    default String stringValue(String key) {
        return value(key, DataCursor::stringValue);
    }
}
