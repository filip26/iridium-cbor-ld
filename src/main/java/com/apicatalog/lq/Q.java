package com.apicatalog.lq;

import java.util.Collection;

public class Q {

    public static boolean isNull(ValueHolder value) {
        return (boolean) value.apply(Functions::isNull);
    }
    
    public static boolean isMap(ValueHolder value) {
        return (boolean) value.apply(Functions::isMap);
    }

    public static boolean isArray(ValueHolder value) {
        return (boolean) value.apply(Functions::isArray);
    }

    public static boolean isScalar(ValueHolder value) {
        return (boolean) value.apply(Functions::isScalar);
    }

    public static boolean isString(ValueHolder value) {
        return (boolean) value.apply(Functions::isString);
    }

    public static boolean isBoolean(ValueHolder value) {
        return (boolean) value.apply(Functions::isBoolean);
    }

    public static boolean contains(ValueHolder map, final String key) {
        return (boolean) map.apply(f -> f.contains(key));
    }

    public static boolean isNumber(ValueHolder value) {
        return (boolean) value.apply(Functions::isNumber);
    }

    public static boolean isEmpty(ValueHolder value) {
        return (boolean) value.apply(Functions::isEmpty);
    }

    
    public static String asString(ValueHolder value) {
        return (String) value.apply(Functions::asString);
    }

    public static ValueHolder value(ValueHolder map, final String key) {
        return (ValueHolder) map.apply(f -> f.entry(key));
    }


    public static Iterable<ValueHolder> iterable(ValueHolder structure) {
        return (Iterable<ValueHolder>) structure.apply(Functions::iterable);
    }

    public static int size(ValueHolder structure) {
        return (int) structure.apply(Functions::size);
    }

    @SuppressWarnings("unchecked")
    public static Collection<String> keys(ValueHolder map) {
        return (Collection<String>) map.apply(Functions::keys);
    }

}
