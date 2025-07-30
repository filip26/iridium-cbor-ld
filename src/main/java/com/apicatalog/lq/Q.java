package com.apicatalog.lq;

import java.util.Collection;

public class Q {

    public static DataType type(Data value) {
        return value == null
                ? null
                : (DataType) value.apply(Functions::type);
    }

    public static boolean contains(Data map, final String key) {
        return (boolean) map.apply(f -> f.contains(key));
    }

    public static boolean isEmpty(Data value) {
        return (boolean) value.apply(Functions::isEmpty);
    }

    public static String string(Data value) {
        return (String) value.apply(Functions::getString);
    }

    public static Data value(Data map, final String key) {
        return (Data) map.apply(f -> f.entry(key));
    }

    @SuppressWarnings("unchecked")
    public static Iterable<Data> iterable(Data array) {
        return (Iterable<Data>) array.apply(Functions::iterable);
    }

    public static int size(Data structure) {
        return (int) structure.apply(Functions::size);
    }

    @SuppressWarnings("unchecked")
    public static Collection<String> keys(Data map) {
        return (Collection<String>) map.apply(Functions::keys);
    }

    public static boolean isScalar(DataType dataType) {
        return dataType == DataType.STRING
                || dataType == DataType.TRUE
                || dataType == DataType.FALSE
                || dataType == DataType.INTEGER
                || dataType == DataType.DECIMAL;
    }

    public static boolean isNumber(DataType dataType) {
        return dataType == DataType.INTEGER
                || dataType == DataType.DECIMAL;
    }

}
