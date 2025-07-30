package com.apicatalog.lq;

import java.util.Collection;
import java.util.function.Function;

public interface Functions {

    Function<?, Boolean> isNull();

    Function<?, Boolean> isEmpty();
    
    Function<?, Boolean> isArray();
    
    Function<?, Boolean> isMap();
    
    Function<?, Boolean> isString();
    Function<?, String> asString();
    
    Function<?, Boolean> isBoolean();
//    Boolean booleanValue(ValueHolder value);

    Function<?, Boolean> isNumber();
    
//    Function<?, Boolean> isInteger();
//    Function<?, Boolean> isDecimal();
    
//    BigInteger integerValue(ValueHolder value);
//    BigDecimal decimalValue(ValueHolder value);

    Function<?, Boolean> isScalar();

    Function<?, Boolean> contains(String key);
    
    Function<?, ValueHolder> entry(String key);
    
    Function<?, Iterable<ValueHolder>> iterable();

    Function<?, Integer> size();
    
    Function<?, Collection<String>> keys();
    
  //return value.apply(m -> ((Predicate)m.isString()).or(
//  (Predicate)m.isBoolean()
//  ));
  
//  || m.isBoolean() || m.isNumber());

//  default boolean isNumber(ValueProvider source) {
//      return isInteger(source) || isDecimal(source);
//  }
//  
//  default boolean isStructure(ValueProvider source) {
//      return isMap(source) || isArray(source);
//  }


//    default boolean isNonEmptyArray(ValueProvider value) {
//        return isArray(value) && !asArray().isEmpty();
//    }
    
//    ArrayCursor asArray();

//    default boolean isNonEmptyMap() {
//        return isMap() && !asMap().isEmpty();
//    }


    
//    MapCursor asMap();
    
//    boolean isArrayItem();
//    ArrayItemCursor asArrayItem();
    
//    boolean isMapEntry();
//    MapEntryCursor asMapEntry();
//    
//    static Stream<ValueCursor> toStream(ValueCursor cursor) {
//        if (cursor.isArray()) {
//            return StreamSupport.stream(cursor.asArray().spliterator(), false).map(ValueCursor.class::cast);
//        }
//
//        return Stream.of(cursor);
//    }
}
