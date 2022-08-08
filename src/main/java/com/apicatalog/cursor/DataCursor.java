package com.apicatalog.cursor;

public interface DataCursor<P> {
    
    public enum DataType {
        Null,
        String,
        Integer,
        Decimal,
        Boolean,
        Array,
        Map,
        Binary
    }

    P parent();

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
    
//    static Stream<DataCursor> toStream(DataCursor cursor) {
//        if (cursor.isArray()) {
//            return StreamSupport.stream(cursor.asArray().spliterator(), false).map(DataCursor.class::cast);
//        }
//
//        return Stream.of(cursor);
//    }
}
