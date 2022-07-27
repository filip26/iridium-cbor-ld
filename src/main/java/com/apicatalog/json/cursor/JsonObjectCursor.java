package com.apicatalog.json.cursor;

import java.util.Collection;

public interface JsonObjectCursor extends JsonValueCursor {

    Collection<String> properies();

    boolean has(String property);

    boolean isNull(String property);

    boolean isString(String propertt);
    boolean isBoolean(String propertt);
    boolean isNumber(String propert);

    default boolean isPrimitive(String propert) {
        return isString(propert) || isBoolean(propert) || isNumber(propert);
    }

    boolean isArray(String property);
    boolean isNonEmptyArray(String property);

    boolean isObject(String property);
    boolean isNonEmptyObject(String property);

    default boolean isStructure(String propert) {
        return isArray(propert) || isObject(propert);
    }

    Boolean booleanValue(String propert);

    Integer integerValue(String propert);

    Long longValue(String propert);

    String stringValue(String propert);

    JsonArrayCursor array(String property);

    JsonObjectCursor object(String property);

    JsonValueCursor value(String property);
}
