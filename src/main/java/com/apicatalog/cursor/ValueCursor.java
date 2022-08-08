package com.apicatalog.cursor;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Optional;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public interface ValueCursor {

    Optional<ValueCursor> parent();

    boolean isNull();

    boolean isString();
    String stringValue();
    
    boolean isBoolean();
    Boolean booleanValue();

    boolean isInteger();
    boolean isDecimal();
    
    BigInteger integerValue();
    BigDecimal decimalValue();

    default boolean isNumber() {
        return isInteger() || isDecimal();
    }
    
    default boolean isScalar() {
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
    
    boolean isArrayItem();
    ArrayItemCursor asArrayItem();
    
    boolean isMapEntry();
    MapEntryCursor asMapEntry();
    
    static Stream<ValueCursor> toStream(ValueCursor cursor) {
        if (cursor.isArray()) {
            return StreamSupport.stream(cursor.asArray().spliterator(), false).map(ValueCursor.class::cast);
        }

        return Stream.of(cursor);
    }
}
