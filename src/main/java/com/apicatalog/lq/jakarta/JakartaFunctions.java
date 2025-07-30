package com.apicatalog.lq.jakarta;

import java.util.Collection;
import java.util.function.Function;

import com.apicatalog.lq.Functions;
import com.apicatalog.lq.ValueHolder;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JakartaFunctions implements Functions {

    @Override
    public Function<JsonValue, Boolean> isNull() {
        return JakartaFunctions::isNull;
    }

    public static final boolean isNull(JsonValue value) {
        return value == null || ValueType.NULL == value.getValueType();
    }

    @Override
    public Function<JsonValue, Boolean> isArray() {
        return JakartaFunctions::isArray;
    }

    public static final boolean isArray(JsonValue value) {
        return value != null && ValueType.ARRAY == value.getValueType();
    }

    @Override
    public Function<JsonObject, Boolean> contains(String key) {
        return object -> object.containsKey(key);
    }

    @Override
    public Function<JsonObject, ValueHolder> entry(String key) {
        return object -> JakartaAdapter.of(object.get(key));
    }

    @Override
    public Function<JsonValue, Boolean> isString() {
        return JakartaFunctions::isString;
    }

    public static final boolean isString(JsonValue value) {
        return value != null && ValueType.STRING == value.getValueType();
    }

    @Override
    public Function<JsonValue, Boolean> isBoolean() {
        return JakartaFunctions::isBoolean;
    }

    public static final boolean isBoolean(JsonValue value) {
        return value != null && (ValueType.FALSE == value.getValueType()
                || ValueType.TRUE == value.getValueType());
    }

    @Override
    public Function<JsonValue, Boolean> isNumber() {
        return JakartaFunctions::isNumber;
    }

    public static final boolean isNumber(JsonValue value) {
        return value != null && (ValueType.NUMBER == value.getValueType());
    }

    @Override
    public Function<JsonValue, Boolean> isScalar() {
        return JakartaFunctions::isScalar;
    }

    public static final boolean isScalar(JsonValue value) {
        return value != null && ValueType.ARRAY != value.getValueType()
                && ValueType.OBJECT != value.getValueType()
                && ValueType.NULL != value.getValueType();
    }

    @Override
    public Function<JsonString, String> asString() {
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
    public Function<JsonValue, Boolean> isMap() {
        return value -> value != null && value.getValueType() == ValueType.OBJECT;
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
    public Function<JsonArray, Iterable<ValueHolder>> iterable() {
        return value -> value.stream().map(JakartaAdapter::of).toList();
    }
}
