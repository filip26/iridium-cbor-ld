package com.apicatalog.cursor.jakarta;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.Supplier;

import com.apicatalog.cursor.AbstractValueCursor;
import com.apicatalog.cursor.MapEntryCursor;
import com.apicatalog.cursor.ValueCursor;

import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JakartaValueCursor extends AbstractValueCursor<JsonValue> {

    public JakartaValueCursor(final JakartaJsonCursor cursor, Supplier<JsonValue> value) {
        super(cursor, value);
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
    public boolean isInteger() {
        return isNumber() && ((JsonNumber)value.get()).isIntegral();
    }
    
    @Override
    public boolean isDecimal() {
        return isNumber() && !((JsonNumber)value.get()).isIntegral();
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
    public BigInteger integerValue() {
        if (!isInteger()) {
            throw new ClassCastException();
        }
        return ((JsonNumber)value.get()).bigIntegerValueExact();
    }

    @Override
    public BigDecimal decimalValue() {
        if (!isDecimal()) {
            throw new ClassCastException();
        }
        return ((JsonNumber)value.get()).bigDecimalValue();
    }

    @Override
    public String stringValue() {
        if (!isString()) {
            throw new ClassCastException();
        }
        return ((JsonString)value.get()).getString();
    }
    
    public static JsonValue toJson(final ValueCursor cursor) {
        if (cursor.isArray()) {
            
            if (cursor.asArray().isEmpty()) {
                return JsonValue.EMPTY_JSON_ARRAY;
            }
            
            final JsonArrayBuilder array = Json.createArrayBuilder();
            
            for (final ValueCursor item : cursor.asArray()) {
                array.add(toJson(item));
            }
            cursor.parent();
            
            return array.build();

        } else if (cursor.isBoolean()) {
            return cursor.booleanValue() ? JsonValue.TRUE : JsonValue.FALSE;
            
        } else if (cursor.isMap()) {
            
            if (cursor.asMap().isEmpty()) {
                return JsonValue.EMPTY_JSON_OBJECT;
            }
            
            final JsonObjectBuilder map = Json.createObjectBuilder();
            
            for (final MapEntryCursor entry : cursor.asMap()) {
                map.add(entry.mapKey().toString(), toJson(entry));
            }
            cursor.parent();
            
            return map.build();
            
        } else if (cursor.isNull()) {
            return JsonValue.NULL;
            
        } else if (cursor.isInteger()) {
            return Json.createValue(cursor.integerValue());
            
        } else if (cursor.isDecimal()) {
            return Json.createValue(cursor.decimalValue());
            
        } else if (cursor.isString()) {
            return Json.createValue(cursor.stringValue());
        }
        
        throw new IllegalStateException();
    }
}
