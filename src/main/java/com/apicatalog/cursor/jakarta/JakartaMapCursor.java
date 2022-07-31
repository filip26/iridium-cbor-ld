package com.apicatalog.cursor.jakarta;

import java.util.Collection;
import java.util.stream.Stream;

import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.ValueCursor;

import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JakartaMapCursor extends JakartaValueCursor implements MapCursor {

    public JakartaMapCursor(final JakartaJsonCursor cursor) {
        super(cursor);
    }

    @Override
    public int size() {
        if (!isObject()) {
            throw new ClassCastException();
        }
        return cursor.value().asJsonObject().size();
    }

    @Override
    public Collection<String> keys() {
        if (!isObject()) {
            throw new ClassCastException();
        }
        return cursor.value().asJsonObject().keySet();
    }

    @Override
    public boolean has(String key) {
        if (!isObject()) {
            throw new ClassCastException();
        }
        return cursor.value().asJsonObject().containsKey(key);
    }

    @Override
    public boolean isNull(String key) {
        if (!isObject()) {
            throw new ClassCastException();
        }
        final JsonValue value = cursor.value().asJsonObject().get(key);
        return value == null || ValueType.NULL.equals(value.getValueType());
    }

    @Override
    public boolean isString(String key) {
        if (!isObject()) {
            throw new ClassCastException();
        }
        final JsonValue value = cursor.value().asJsonObject().get(key);
        return value != null && ValueType.STRING.equals(value.getValueType());
    }

    @Override
    public boolean isBoolean(String key) {
        if (!isObject()) {
            throw new ClassCastException();
        }
        final JsonValue value = cursor.value().asJsonObject().get(key);
        return value != null && (
            ValueType.TRUE.equals(value.getValueType())
            || ValueType.FALSE.equals(value.getValueType())
            );
    }

    @Override
    public boolean isNumber(String key) {
        if (!isObject()) {
            throw new ClassCastException();
        }
    
        final JsonValue value = cursor.value().asJsonObject().get(key);
        return value != null && ValueType.NUMBER.equals(value.getValueType());
    }

    @Override
    public boolean isArray(String key) {
        if (!isObject()) {
            throw new ClassCastException();
        }
        final JsonValue value = cursor.value().asJsonObject().get(key);
        return value != null && ValueType.ARRAY.equals(value.getValueType());
    }

    @Override
    public boolean isNonEmptyArray(String key) {
        if (!isObject()) {
            throw new ClassCastException();
        }
    
        final JsonValue value = cursor.value().asJsonObject().get(key);
        return value != null
            && ValueType.ARRAY.equals(value.getValueType())
            && !value.asJsonArray().isEmpty();
    }

    @Override
    public boolean isObject(String key) {
        if (!isObject()) {
            throw new ClassCastException();
        }
        final JsonValue value = cursor.value().asJsonObject().get(key);
        return value != null && ValueType.OBJECT.equals(value.getValueType());
    }

    @Override
    public boolean isNonEmptyObject(String key) {
        if (!isObject()) {
            throw new ClassCastException();
        }
    
        final JsonValue value = cursor.value().asJsonObject().get(key);
        return value != null
            && ValueType.OBJECT.equals(value.getValueType())
            && !value.asJsonObject().isEmpty();
    }

    @Override
    public Boolean booleanValue(String key) {
        if (!isObject()) {
            throw new ClassCastException();
        }
    
        final JsonValue value = cursor.value().asJsonObject().get(key);
    
        if (ValueType.TRUE.equals(value.getValueType())) {
            return Boolean.TRUE;
        }
    
        if (ValueType.FALSE.equals(value.getValueType())) {
            return Boolean.FALSE;
        }
    
        throw new ClassCastException();
    }

    @Override
    public Integer integerValue(final String key) {
        if (!isNumber(key)) {
            throw new IllegalArgumentException();
        }
        return ((JsonNumber)cursor.value().asJsonObject().get(key)).intValueExact();
    }

    @Override
    public Long longValue(final String key) {
        if (!isNumber(key)) {
            throw new IllegalArgumentException();
        }
        return ((JsonNumber)cursor.value().asJsonObject().get(key)).longValueExact();
    }

    @Override
    public String stringValue(final String key) {
        if (!isString(key)) {
            throw new IllegalArgumentException();
        }
        return cursor.value().asJsonObject().getString(key, null);
    }

    @Override
    public ArrayCursor array(final String key) {
        if (!isArray(key)) {
            throw new IllegalArgumentException();
        }
    
        return cursor.next(cursor.value().asJsonObject().get(key).asJsonArray());
    }

    @Override
    public MapCursor object(final String key) {
        if (!isObject(key)) {
            throw new IllegalArgumentException();
        }
    
        return cursor.next(cursor.value().asJsonObject().get(key).asJsonObject());
    }

    @Override
    public ValueCursor value(final String key) {
        if (!isObject()) {
            throw new IllegalArgumentException();
        }
    
        return cursor.next(cursor.value().asJsonObject().get(key));
    }
    
    @Override
    public MapCursor clone() {
        return cursor.clone().mapCursor();
    }
    
    @Override
    public Stream<ValueCursor> stream(String key) {
        final MapCursor clone = this.clone();

        if (clone.isArray(key)) {
            return clone.array(key).stream();
        }

        return Stream.of(clone.value(key));
    }

    
    public JsonObject getJsonObject() {
        return (JsonObject) cursor.value();
    }

    
    
    @Override
    public boolean has(int key) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isNull(int key) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isString(int key) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isBoolean(int key) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isNumber(int key) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isArray(int key) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isNonEmptyArray(int key) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isObject(int key) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isNonEmptyObject(int key) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Boolean booleanValue(int key) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Integer integerValue(int key) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Long longValue(int key) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String stringValue(int key) {
        // TODO Auto-generated method stub
        return null;
    }
}
