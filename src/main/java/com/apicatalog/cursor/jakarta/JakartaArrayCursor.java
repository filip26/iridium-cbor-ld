package com.apicatalog.cursor.jakarta;

import java.util.Iterator;
import java.util.stream.Stream;

import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.ValueCursor;

import jakarta.json.JsonValue.ValueType;

public class JakartaArrayCursor extends JakartaValueCursor implements ArrayCursor {
    
    public JakartaArrayCursor(final JakartaJsonCursor cursor) {
        super(cursor);
    }

    @Override
    public int size() {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return cursor.value().asJsonArray().size();
    }

    @Override
    public boolean isNull(int i) {
        if (!isArray()) {
            throw new ClassCastException();
        }
    
        return ValueType.NULL.equals(cursor.value().asJsonArray().get(i).getValueType());
    }

    @Override
    public boolean isString(int i) {
        if (!isArray()) {
            throw new ClassCastException();
        }
    
        return ValueType.STRING.equals(cursor.value().asJsonArray().get(i).getValueType());
    }

    @Override
    public boolean isBoolean(int i) {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return ValueType.TRUE.equals(cursor.value().asJsonArray().get(i).getValueType())
            || ValueType.FALSE.equals(cursor.value().asJsonArray().get(i).getValueType());
    }

    @Override
    public boolean isNumber(int i) {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return ValueType.NUMBER.equals(cursor.value().asJsonArray().get(i).getValueType());
    }

    @Override
    public boolean isArray(int i) {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return ValueType.ARRAY.equals(cursor.value().asJsonArray().get(i).getValueType());
    }

    @Override
    public boolean isNonEmptyArray(int i) {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return ValueType.ARRAY.equals(cursor.value().asJsonArray().get(i).getValueType())
            && !cursor.value().asJsonArray().getJsonArray(i).isEmpty();
    }

    @Override
    public boolean isObject(int i) {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return ValueType.OBJECT.equals(cursor.value().asJsonArray().get(i).getValueType());
    }

    @Override
    public boolean isNonEmptyObject(int i) {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return ValueType.OBJECT.equals(cursor.value().asJsonArray().get(i).getValueType())
            && !cursor.value().asJsonArray().getJsonObject(i).isEmpty();
    }

    @Override
    public Boolean booleanValue(int i) {
        if (!isArray()) {
            throw new ClassCastException();
        }
    
        if (ValueType.TRUE.equals(cursor.value().asJsonArray().get(i).getValueType())) {
            return Boolean.TRUE;
        }
        if (ValueType.FALSE.equals(cursor.value().asJsonArray().get(i).getValueType())) {
            return Boolean.FALSE;
        }
    
        throw new ClassCastException();
    }

    @Override
    public Integer integerValue(int i) {
        if (!isNumber(i)) {
            throw new ClassCastException();
        }
        return cursor.value().asJsonArray().getJsonNumber(i).intValueExact();
    }

    @Override
    public Long longValue(int i) {
        if (!isNumber(i)) {
            throw new ClassCastException();
        }
        return cursor.value().asJsonArray().getJsonNumber(i).longValueExact();
    }

    @Override
    public String stringValue(int i) {
        if (!isString(i)) {
            throw new ClassCastException();
        }
        return cursor.value().asJsonArray().getJsonString(i).getString();
    }

    @Override
    public ArrayCursor array(int i) {
        if (!isArray(i)) {
            throw new IllegalArgumentException();
        }    
        return cursor.next(cursor.value().asJsonArray().get(i).asJsonArray());
    }

    @Override
    public MapCursor object(int i) {
        if (!isObject(i)) {
            throw new IllegalArgumentException();
        }
        return cursor.next(cursor.value().asJsonArray().get(i).asJsonObject());
    }

    @Override
    public ValueCursor value(int i) {
        if (!isArray()) {
            throw new ClassCastException();
        }    
        return cursor.next(cursor.value().asJsonArray().get(i));
    }

    @Override
    protected ArrayCursor clone() {
        return cursor.clone().arrayCursor();
    }

    @Override
    public Iterator<ValueCursor> iterator() {
        if (!isArray()) {
            throw new ClassCastException();
        }
        
        final ArrayCursor clone = this.clone();
        final int size = size();
        
        return new Iterator<ValueCursor>() {

            int index = 0;

            @Override
            public boolean hasNext() {
                return index < size;
            }

            @Override
            public ValueCursor next() {
                return clone.value(index++);
            }
        };
    }
    
    @Override
    public Stream<ValueCursor> stream() {
        return Stream.of(this);
    }
    
}
