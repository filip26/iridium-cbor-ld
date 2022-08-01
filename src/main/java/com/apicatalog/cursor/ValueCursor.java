package com.apicatalog.cursor;

import java.util.Optional;

public interface ValueCursor {

    Optional<ValueCursor> parent();

    boolean isNull();

    boolean isString();
    boolean isBoolean();
    boolean isNumber();

    default boolean isScalar() {
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

    MapCursor asObject();
    ArrayCursor asArray();

    ValueCursor clone();
}
