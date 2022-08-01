package com.apicatalog.cursor;

public interface ArrayCursor extends StructureCursor, Iterable<ValueCursor> {

    default boolean isNull(int index) {
        boolean isNull = value(index).isNull();
        parent();
        return isNull;
    }

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

    ArrayCursor array(int index);
    MapCursor object(int index);
    ValueCursor value(int index);
}
