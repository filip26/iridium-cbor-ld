package com.apicatalog.json.cursor.jakarta;

import java.util.Collection;
import java.util.Optional;

import com.apicatalog.json.cursor.JsonArrayCursor;
import com.apicatalog.json.cursor.JsonCursor;
import com.apicatalog.json.cursor.JsonObjectCursor;
import com.apicatalog.json.cursor.JsonValueCursor;

import jakarta.json.JsonNumber;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JakartaJsonCursor implements JsonCursor {

    //TODO use dynamic lazy index
    final JsonValue[] path;
    final Object[] ids;
//    final String[] names;
//    final int[] indicies;
    
    int index;

    public JakartaJsonCursor(JsonValue root, int maxDepth) {
	this.path = new JsonString[maxDepth];
	this.path[0] = root;
	this.ids = new Object[maxDepth];
	this.ids[0] = null;
	this.index = 0;
    }
    
    @Override
    public Optional<JsonValueCursor> parent() {
	if (index == 0) {
	    return Optional.empty();
	}

	index--;
	return Optional.of(this);
    }

    @Override
    public boolean isNull() {
	return ValueType.NULL.equals(path[index].getValueType());
    }

    @Override
    public boolean isString() {
	return ValueType.STRING.equals(path[index].getValueType());
    }

    @Override
    public boolean isBoolean() {
	//TODO
	return false;
    }

    @Override
    public boolean isNumber() {
	return ValueType.NUMBER.equals(path[index].getValueType());
    }

    @Override
    public boolean isArray() {
	return ValueType.ARRAY.equals(path[index].getValueType());
    }

    @Override
    public boolean isNonEmptyArray() {
	return isArray() && !path[index].asJsonArray().isEmpty();
    }

    @Override
    public boolean isObject() {
	return ValueType.OBJECT.equals(path[index].getValueType());
    }

    @Override
    public boolean isNonEmptyObject() {
	return isObject() && !path[index].asJsonObject().isEmpty();
    }

    @Override
    public Boolean booleanValue() {
	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public Integer integerValue() {
	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public Long longValue() {
	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public String stringValue() {
	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public JsonObjectCursor asObject() {
	if (!isObject()) {
	    throw new ClassCastException();
	}
	return this;
    }

    @Override
    public JsonArrayCursor asArray() {
	if (!isArray()) {
	    throw new ClassCastException();
	}
	return this;
    }

    @Override
    public int size() {
	if (!isArray()) {
	    throw new ClassCastException();
	}
	return path[index].asJsonArray().size();
    }

    @Override
    public boolean isNull(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isString(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isBoolean(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isNumber(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isArray(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isNonEmptyArray(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isObject(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isNonEmptyObject(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public Boolean booleanValue(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public Integer integerValue(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public Long longValue(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public String stringValue(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public JsonArrayCursor array(int index) {
	if (!isArray(index)) {
	    throw new IllegalArgumentException();
	}

	path[index + 1] = path[index].asJsonArray().get(index);  
	index++;

	return this;
    }

    @Override
    public JsonObjectCursor object(int index) {
	if (!isObject(index)) {
	    throw new IllegalArgumentException();
	}

	path[index + 1] = path[index].asJsonArray().get(index);  
	index++;

	return this;
    }

    @Override
    public JsonCursor value(int index) {
	if (!isArray()) {
	    throw new IllegalArgumentException();
	}

	path[index + 1] = path[index].asJsonArray().get(index);  
	index++;

	return this;
    }

    @Override
    public Collection<String> properies() {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public boolean has(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isNull(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isString(String propertt) {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isBoolean(String propertt) {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isNumber(String propert) {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isArray(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isNonEmptyArray(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isObject(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public boolean isNonEmptyObject(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return false;
    }

    @Override
    public Boolean booleanValue(String propert) {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public Integer integerValue(final String property) {
	if (!isNumber(property)) {
	    throw new IllegalArgumentException();
	}
	return ((JsonNumber)path[index]).intValueExact();
    }

    @Override
    public Long longValue(final String property) {
	if (!isNumber(property)) {
	    throw new IllegalArgumentException();
	}
	return ((JsonNumber)path[index]).longValueExact();
    }

    @Override
    public String stringValue(final String property) {
	if (!isString(property)) {
	    throw new IllegalArgumentException();
	}
	return ((JsonString)path[index]).getString();
    }

    @Override
    public JsonArrayCursor array(final String property) {
	if (!isArray(property)) {
	    throw new IllegalArgumentException();
	}

	path[index + 1] = path[index].asJsonObject().get(property);  
	index++;

	return this;
    }

    @Override
    public JsonObjectCursor object(final String property) {
	if (!isObject(property)) {
	    throw new IllegalArgumentException();
	}

	path[index + 1] = path[index].asJsonObject().get(property);
	ids[index + 1] = property;
	index++;

	return this;
    }

    @Override
    public JsonCursor value(final String property) {
	if (!isObject()) {
	    throw new IllegalArgumentException();
	}
	
	path[index + 1] = path[index].asJsonObject().get(property);
	ids[index + 1] = property;
	index++;
	
	return this;
    }

    @Override
    public JsonCursor clone() {
	return new JakartaJsonCursor(path[index], path.length);
    }

    @Override
    public JsonCursor reset() {
	index = 0;
	return this;
    }    
}
