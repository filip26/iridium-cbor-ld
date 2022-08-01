package com.apicatalog.cursor.jakarta;

import java.util.Optional;

import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.ValueCursor;

import jakarta.json.JsonNumber;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JakartaValueCursor implements ValueCursor {

    protected final JakartaJsonCursor cursor;

    public JakartaValueCursor(final JakartaJsonCursor cursor) {
        this.cursor = cursor;
    }

    @Override
    public Optional<ValueCursor> parent() {
        return cursor.prev() 
                ? Optional.of(this)
                : Optional.empty();
    }

    @Override
    public boolean isNull() {
        return ValueType.NULL.equals(cursor.value().getValueType());
    }

    @Override
    public boolean isString() {
        return ValueType.STRING.equals(cursor.value().getValueType());
    }

    @Override
    public boolean isBoolean() {
        return ValueType.FALSE.equals(cursor.value().getValueType()) || ValueType.TRUE.equals(cursor.value().getValueType());
    }

    @Override
    public boolean isNumber() {
        return ValueType.NUMBER.equals(cursor.value().getValueType());
    }

    @Override
    public boolean isArray() {
        return ValueType.ARRAY.equals(cursor.value().getValueType());
    }

    @Override
    public boolean isNonEmptyArray() {
        return isArray() && !cursor.value().asJsonArray().isEmpty();
    }

    @Override
    public boolean isObject() {
        return ValueType.OBJECT.equals(cursor.value().getValueType());
    }

    @Override
    public boolean isNonEmptyObject() {
        return isObject() && !cursor.value().asJsonObject().isEmpty();
    }

    @Override
    public Boolean booleanValue() {
        if (ValueType.TRUE.equals(cursor.value().getValueType())) {
            return Boolean.TRUE;
        }
        if (ValueType.FALSE.equals(cursor.value().getValueType())) {
            return Boolean.FALSE;
        }
        throw new ClassCastException();
    }

    @Override
    public Integer integerValue() {
        if (!isNumber()) {
            throw new ClassCastException();
        }
        return ((JsonNumber)cursor.value()).intValueExact();
    }

    @Override
    public Long longValue() {
        if (!isNumber()) {
            throw new ClassCastException();
        }
        return ((JsonNumber)cursor.value()).longValueExact();
    }

    @Override
    public String stringValue() {
        if (!isString()) {
            throw new ClassCastException();
        }
        return ((JsonString)cursor.value()).getString();
    }

    @Override
    public MapCursor asObject() {
        if (!isObject()) {
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

    @Override
    public ValueCursor clone() {
        return cursor.clone().cursor();
    }
    
    public JsonValue getJsonValue() {
        return cursor.value();
    }
}
