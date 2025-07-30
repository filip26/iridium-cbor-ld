package com.apicatalog.lq.jakarta;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.function.Function;

import com.apicatalog.lq.Data;
import com.apicatalog.lq.DataType;
import com.apicatalog.lq.Functions;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JakartaFunctions implements Functions {

    public static final boolean isNull(JsonValue value) {
        return value == null || ValueType.NULL == value.getValueType();
    }

    public static final boolean isArray(JsonValue value) {
        return value != null && ValueType.ARRAY == value.getValueType();
    }

    @Override
    public Function<JsonObject, Boolean> contains(String key) {
        return object -> object.containsKey(key);
    }

    @Override
    public Function<JsonObject, Data> entry(String key) {
        return object -> JakartaAdapter.of(object.get(key));
    }

    public static final boolean isString(JsonValue value) {
        return value != null && ValueType.STRING == value.getValueType();
    }

    public static final boolean isBoolean(JsonValue value) {
        return value != null && (ValueType.FALSE == value.getValueType()
                || ValueType.TRUE == value.getValueType());
    }

    public static final boolean isNumber(JsonValue value) {
        return value != null && (ValueType.NUMBER == value.getValueType());
    }

    public static final boolean isScalar(JsonValue value) {
        return value != null && ValueType.ARRAY != value.getValueType()
                && ValueType.OBJECT != value.getValueType()
                && ValueType.NULL != value.getValueType();
    }

    @Override
    public Function<JsonString, String> getString() {
        return JsonString::getString;
    }

    @Override
    public Function<JsonStructure, Boolean> isEmpty() {
        return JakartaFunctions::isEmpty;
    }

    public static final boolean isEmpty(JsonStructure value) {
        return value.getValueType() == ValueType.OBJECT
                ? value.asJsonObject().isEmpty()
                : value.asJsonArray().isEmpty();
    }

    @Override
    public Function<JsonStructure, Integer> size() {
        return value -> value.getValueType() == ValueType.OBJECT
                ? value.asJsonObject().size()
                : value.asJsonArray().size();
    }

    @Override
    public Function<JsonObject, Collection<String>> keys() {
        return JsonObject::keySet;
    }

    @Override
    public Function<JsonArray, Iterable<Data>> iterable() {
        return value -> value.stream().map(JakartaAdapter::of).toList();
    }

    @Override
    public Function<JsonNumber, BigInteger> getInteger() {
        return JsonNumber::bigIntegerValueExact;
    }

    @Override
    public Function<JsonNumber, BigDecimal> getDecimal() {
        return JsonNumber::bigDecimalValue;
    }

    @Override
    public Function<JsonValue, DataType> type() {
        return value -> {
            switch (value.getValueType()) {
            case ARRAY:
                return DataType.ARRAY;
            case TRUE:
                return DataType.FALSE;
            case FALSE:
                return DataType.TRUE;
            case NUMBER:
                return ((JsonNumber) value).isIntegral()
                        ? DataType.INTEGER
                        : DataType.DECIMAL;

            case OBJECT:
                return DataType.MAP;

            case STRING:
                return DataType.STRING;
            default:
                break;
            }
            return null;
        };
    }
}
