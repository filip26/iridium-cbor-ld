package com.apicatalog.lq.jakarta;

import java.util.function.Function;

import com.apicatalog.lq.Data;
import com.apicatalog.lq.DataType;
import com.apicatalog.lq.Functions;
import com.apicatalog.lq.Q;

import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

@Deprecated
public class JakartaAdapter {

    public static final Functions VALUE_PROVIDER = new JakartaFunctions();

    @SuppressWarnings("unchecked")
    public static final Data of(JsonValue value) {
        return JakartaFunctions.isNull(value)
                ? null
                : t -> ((Function<JsonValue, ?>) t.apply(VALUE_PROVIDER)).apply(value);
    }

    public static JsonValue toJson(Data value) {

        if (value == null) {
            return JsonValue.NULL;
        }

        final DataType dataType = Q.type(value);

        switch (dataType) {

        case STRING:
            return Json.createValue(Q.string(value));

        case INTEGER:
            return Json.createValue(Q.integer(value));

        case DECIMAL:
            return Json.createValue(Q.decimal(value));

        case TRUE:
            return JsonValue.TRUE;

        case FALSE:
            return JsonValue.FALSE;

        case ARRAY:
            if (Q.isEmpty(value)) {
                return JsonValue.EMPTY_JSON_ARRAY;
            }

            final JsonArrayBuilder array = Json.createArrayBuilder();

            for (final Data item : Q.iterable(value)) {
                array.add(toJson(item));
            }

            return array.build();

        case MAP:
            if (Q.isEmpty(value)) {
                return JsonValue.EMPTY_JSON_OBJECT;
            }

            final JsonObjectBuilder map = Json.createObjectBuilder();

            for (final String key : Q.keys(value)) {

                Data entry = Q.value(value, key);

                map.add(key, toJson(entry));
            }

            return map.build();
            
        default:
            break;
        }

        throw new IllegalStateException("An unsupported data type '" + dataType + "'.");
    }

}
