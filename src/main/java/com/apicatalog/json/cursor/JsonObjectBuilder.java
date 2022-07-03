package com.apicatalog.json.cursor;

public interface JsonObjectBuilder {

    JsonObjectBuilder set(String property, String value);

    JsonObjectBuilder set(String property, boolean value);

    JsonObjectBuilder set(String property, long value);

    JsonObjectBuilder createObject(String property);

    //JsonArrayCursor createArray(String property);

    JsonObjectBuilder remove(String property);
}
