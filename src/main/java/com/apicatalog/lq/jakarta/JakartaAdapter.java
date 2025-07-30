package com.apicatalog.lq.jakarta;

import java.util.Collection;
import java.util.function.Function;

import com.apicatalog.cursor.MapEntryCursor;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.lq.Functions;
import com.apicatalog.lq.Q;
import com.apicatalog.lq.ValueHolder;

import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

public class JakartaAdapter {

    public static final Functions VALUE_PROVIDER = new JakartaFunctions();
        
    @SuppressWarnings("unchecked")
    public static final ValueHolder of(JsonValue value) {
        return t -> ((Function<JsonValue, ?>) t.apply(VALUE_PROVIDER)).apply(value);
    }

    public static JsonValue toJson(ValueHolder value) {
        if (Q.isArray(value)) {
            
            if (Q.isEmpty(value)) {
                return JsonValue.EMPTY_JSON_ARRAY;
            }
            
            final JsonArrayBuilder array = Json.createArrayBuilder();
            
            for (final ValueHolder item : Q.iterable(value)) {
                array.add(toJson(item));
            }
            
            return array.build();

        } else if (Q.isBoolean(value)) {
//            return Q.booleanValue(value) ? JsonValue.TRUE : JsonValue.FALSE;
            
        } else if (Q.isMap(value)) {
            
            if (Q.isEmpty(value)) {
                return JsonValue.EMPTY_JSON_OBJECT;
            }
            
            
            final JsonObjectBuilder map = Json.createObjectBuilder();
            
            for (final String key : Q.keys(value)) {
                
                ValueHolder entry = Q.value(value, key);
                
                map.add(key, toJson(entry));
            }
            
            return map.build();
            
        } else if (Q.isNull(value)) {
            return JsonValue.NULL;
            
//        } else if (cursor.isInteger()) {
//            return Json.createValue(cursor.integerValue());
//            
//        } else if (cursor.isDecimal()) {
//            return Json.createValue(cursor.decimalValue());
//            
//        } else if (cursor.isString()) {
//            return Json.createValue(cursor.stringValue());
        }
        
        throw new IllegalStateException();
    }

}
