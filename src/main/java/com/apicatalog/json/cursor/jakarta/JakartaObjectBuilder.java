package com.apicatalog.json.cursor.jakarta;

import com.apicatalog.json.cursor.JsonObjectBuilder;

public class JakartaObjectBuilder implements JsonObjectBuilder {

    @Override
    public JsonObjectBuilder set(String property, String value) {
	return this;
    }

    @Override
    public JsonObjectBuilder set(String property, boolean value) {
	return this;
    }

    @Override
    public JsonObjectBuilder set(String property, long value) {
	return this;
    }

    @Override
    public JsonObjectBuilder createObject(String property) {
	return this;
    }

    @Override
    public JsonObjectBuilder remove(String property) {
	return this;
    }
    
}
