package com.apicatalog.cursor.jakarta;

import java.util.Optional;
import java.util.function.Supplier;

import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.ArrayItemCursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.MapEntryCursor;
import com.apicatalog.cursor.ValueCursor;

import jakarta.json.JsonNumber;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JakartaValueCursor implements ValueCursor {

    protected final JakartaJsonCursor cursor;
    protected final Supplier<JsonValue> value;

    public JakartaValueCursor(final JakartaJsonCursor cursor, Supplier<JsonValue> value) {
        this.cursor = cursor;
        this.value = value;
    }

    @Override
    public Optional<ValueCursor> parent() {
        return cursor.prev() 
                ? Optional.of(this)
                : Optional.empty();
    }

    @Override
    public boolean isNull() {
        return ValueType.NULL.equals(value.get().getValueType());
    }

    @Override
    public boolean isString() {
        return ValueType.STRING.equals(value.get().getValueType());
    }

    @Override
    public boolean isBoolean() {
        return ValueType.FALSE.equals(value.get().getValueType()) || ValueType.TRUE.equals(value.get().getValueType());
    }

    @Override
    public boolean isNumber() {
        return ValueType.NUMBER.equals(value.get().getValueType());
    }

    @Override
    public boolean isArray() {
        return ValueType.ARRAY.equals(value.get().getValueType());
    }

    @Override
    public boolean isNonEmptyArray() {
        return isArray() && !value.get().asJsonArray().isEmpty();
    }

    @Override
    public boolean isMap() {
        return ValueType.OBJECT.equals(value.get().getValueType());
    }

    @Override
    public boolean isNonEmptyMap() {
        return isMap() && !value.get().asJsonObject().isEmpty();
    }

    @Override
    public Boolean booleanValue() {
        if (ValueType.TRUE.equals(value.get().getValueType())) {
            return Boolean.TRUE;
        }
        if (ValueType.FALSE.equals(value.get().getValueType())) {
            return Boolean.FALSE;
        }
        throw new ClassCastException();
    }

    @Override
    public Integer integerValue() {
        if (!isNumber()) {
            throw new ClassCastException();
        }
        return ((JsonNumber)value.get()).intValueExact();
    }

    @Override
    public Long longValue() {
        if (!isNumber()) {
            throw new ClassCastException();
        }
        return ((JsonNumber)value.get()).longValueExact();
    }

    @Override
    public String stringValue() {
        if (!isString()) {
            throw new ClassCastException();
        }
        return ((JsonString)value.get()).getString();
    }

    @Override
    public MapCursor asMap() {
        if (!isMap()) {
            throw new ClassCastException();
        }
        return cursor.mapCursor();
    }

    @Override
    public ArrayCursor asArray() {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return cursor.arrayCursor();
    }

    @Override
    public String toString() {
       return new StringBuilder()
           .append(JakartaValueCursor.class.getSimpleName())
           .append('[')
           .append("cursor=")
           .append(cursor.toString())
           .append(']')
           .toString();
    }

//    @Override
//    public ValueCursor clone() {
//        return cursor.clone().valueCursor();
//    }
    
    public JsonValue getJsonValue() {
        return value.get();
    }

    @Override
    public boolean isArrayItem() {
        return cursor.isArrayItem();
    }

    @Override
    public ArrayItemCursor asArrayItem() {
        return cursor.arrayItemCursor();
    }

    @Override
    public boolean isMapEntry() {
        return cursor.isMapEntry();
    }

    @Override
    public MapEntryCursor asMapEntry() {
        return cursor.mapEntryCursor();
    }
}
