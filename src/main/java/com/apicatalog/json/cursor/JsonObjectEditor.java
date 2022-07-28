package com.apicatalog.json.cursor;

public interface JsonObjectEditor extends JsonValueEditor {

    JsonObjectEditor set(String property, String value);

    JsonObjectEditor set(String property, boolean value);

    JsonObjectEditor set(String property, long value);
    
    JsonObjectEditor setNull(String property);

    JsonValueEditor edit(String property);
    
    JsonObjectEditor editObject(String property);

    //JsonArrayCursor createArray(String property);

    JsonObjectEditor remove(String property);
    
}
