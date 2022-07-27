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
	this.path = new JsonValue[maxDepth];
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
	return ValueType.FALSE.equals(path[index].getValueType()) || ValueType.TRUE.equals(path[index].getValueType());
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
	if (ValueType.TRUE.equals(path[index].getValueType())) {
	    return Boolean.TRUE;
	}
	if (ValueType.FALSE.equals(path[index].getValueType())) {
	    return Boolean.FALSE;
	}
	throw new ClassCastException();
    }

    @Override
    public Integer integerValue() {
	if (!isNumber()) {
	    throw new ClassCastException();
	}
	return ((JsonNumber)path[index]).intValueExact();
    }

    @Override
    public Long longValue() {
	if (!isNumber()) {
	    throw new ClassCastException();
	}
	return ((JsonNumber)path[index]).longValueExact();
    }

    @Override
    public String stringValue() {
	if (!isString()) {
	    throw new ClassCastException();
	}
	return ((JsonString)path[index]).getString();
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
	
	return ValueType.NULL.equals(path[index].asJsonArray().get(index).getValueType());
    }

    @Override
    public boolean isString(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}

	return ValueType.STRING.equals(path[index].asJsonArray().get(index).getValueType());
    }

    @Override
    public boolean isBoolean(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}
	return ValueType.TRUE.equals(path[index].asJsonArray().get(index).getValueType())
		|| ValueType.FALSE.equals(path[index].asJsonArray().get(index).getValueType());
    }

    @Override
    public boolean isNumber(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}
	return ValueType.NUMBER.equals(path[index].asJsonArray().get(index).getValueType());
    }

    @Override
    public boolean isArray(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}
	return ValueType.ARRAY.equals(path[index].asJsonArray().get(index).getValueType());
    }

    @Override
    public boolean isNonEmptyArray(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}
	return ValueType.ARRAY.equals(path[index].asJsonArray().get(index).getValueType())
		&& !path[index].asJsonArray().getJsonArray(index).isEmpty();
    }

    @Override
    public boolean isObject(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}
	return ValueType.OBJECT.equals(path[index].asJsonArray().get(index).getValueType());
    }

    @Override
    public boolean isNonEmptyObject(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}
	return ValueType.OBJECT.equals(path[index].asJsonArray().get(index).getValueType())
		&& !path[index].asJsonArray().getJsonObject(index).isEmpty();
    }

    @Override
    public Boolean booleanValue(int index) {
	if (!isArray()) {
	    throw new ClassCastException();
	}
	
	if (ValueType.TRUE.equals(path[index].asJsonArray().get(index).getValueType())) {
	    return Boolean.TRUE;
	}
	if (ValueType.FALSE.equals(path[index].asJsonArray().get(index).getValueType())) {
	    return Boolean.FALSE;
	}

	throw new ClassCastException();
    }

    @Override
    public Integer integerValue(int index) {
	if (!isNumber(index)) {
	    throw new ClassCastException();
	}
	return path[index].asJsonArray().getJsonNumber(index).intValueExact();
    }

    @Override
    public Long longValue(int index) {
	if (!isNumber(index)) {
	    throw new ClassCastException();
	}
	return path[index].asJsonArray().getJsonNumber(index).longValueExact();
    }

    @Override
    public String stringValue(int index) {
	if (!isString(index)) {
	    throw new ClassCastException();
	}
	return path[index].asJsonArray().getJsonString(index).getString();
    }

    @Override
    public JsonArrayCursor array(int _index) {
	if (!isArray(_index)) {
	    throw new IllegalArgumentException();
	}

	path[index + 1] = path[index].asJsonArray().get(_index);  
	index++;

	return this;
    }

    @Override
    public JsonObjectCursor object(int _index) {
	if (!isObject(_index)) {
	    throw new IllegalArgumentException();
	}

	path[index + 1] = path[index].asJsonArray().get(_index);  
	index++;

	return this;
    }

    @Override
    public JsonCursor value(int _index) {
	if (!isArray()) {
	    throw new IllegalArgumentException();
	}

	path[index + 1] = path[index].asJsonArray().get(_index);  
	index++;

	return this;
    }

    @Override
    public Collection<String> properies() {
	if (!isObject()) {
	    throw new ClassCastException();
	}
	return path[index].asJsonObject().keySet();
    }

    @Override
    public boolean has(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}
	return path[index].asJsonObject().containsKey(property);
    }

    @Override
    public boolean isNull(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}	
	final JsonValue value = path[index].asJsonObject().get(property);
	return value == null || ValueType.NULL.equals(value.getValueType());
    }

    @Override
    public boolean isString(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}
	final JsonValue value = path[index].asJsonObject().get(property);
	return value != null && ValueType.STRING.equals(value.getValueType());
    }

    @Override
    public boolean isBoolean(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}
	final JsonValue value = path[index].asJsonObject().get(property);
	return value != null && (
		ValueType.TRUE.equals(value.getValueType())
		|| ValueType.FALSE.equals(value.getValueType())
		);
    }

    @Override
    public boolean isNumber(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	final JsonValue value = path[index].asJsonObject().get(property);
	return value != null && ValueType.NUMBER.equals(value.getValueType());
    }

    @Override
    public boolean isArray(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}
	final JsonValue value = path[index].asJsonObject().get(property);
	return value != null && ValueType.ARRAY.equals(value.getValueType());
    }

    @Override
    public boolean isNonEmptyArray(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}
	
	final JsonValue value = path[index].asJsonObject().get(property);
	return value != null 
		&& ValueType.ARRAY.equals(value.getValueType())
		&& !value.asJsonArray().isEmpty();
    }

    @Override
    public boolean isObject(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}
	final JsonValue value = path[index].asJsonObject().get(property);
	return value != null && ValueType.OBJECT.equals(value.getValueType());
    }

    @Override
    public boolean isNonEmptyObject(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}

	final JsonValue value = path[index].asJsonObject().get(property);
	return value != null 
		&& ValueType.OBJECT.equals(value.getValueType())
		&& !value.asJsonObject().isEmpty();
    }

    @Override
    public Boolean booleanValue(String property) {
	if (!isObject()) {
	    throw new ClassCastException();
	}
	
	final JsonValue value = path[index].asJsonObject().get(property);
	
	if (ValueType.TRUE.equals(value.getValueType())) {
	    return Boolean.TRUE;
	}
	
	if (ValueType.FALSE.equals(value.getValueType())) {
	    return Boolean.FALSE;
	}

	throw new ClassCastException();
    }

    @Override
    public Integer integerValue(final String property) {
	if (!isNumber(property)) {
	    throw new IllegalArgumentException();
	}
	return ((JsonNumber)path[index].asJsonObject().get(property)).intValueExact();
    }

    @Override
    public Long longValue(final String property) {
	if (!isNumber(property)) {
	    throw new IllegalArgumentException();
	}
	return ((JsonNumber)path[index].asJsonObject().get(property)).longValueExact();
    }

    @Override
    public String stringValue(final String property) {
	if (!isString(property)) {
	    throw new IllegalArgumentException();
	}
	return path[index].asJsonObject().getString(property, null);
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
    
    @Override
    public String toString() {
       return new StringBuilder()
	       .append(JakartaJsonCursor.class.getSimpleName())
	       .append('[')
	       .append("index=")
	       .append(index)
	       .append(", value=")
	       .append(path[index])
	       .append(']')
	       .toString();
    }
}
