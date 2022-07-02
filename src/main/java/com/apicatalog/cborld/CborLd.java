package com.apicatalog.cborld;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map.Entry;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public final class CborLd {

    
    public static final byte[] encode(JsonObject compactedDocument) {
	
	try {
	    
	    final Collection<String> contexts = getReferencedContexts(compactedDocument, new HashSet<>());
	    
	    
	    
	} catch (IllegalArgumentException e) {
	    
	    // non compressable context
	}
	
	return null;
    }
    
    public static final JsonObject decode(byte[] encodedDocument) {
	return null;
    }
    
    
    static final Collection<String> getReferencedContexts(final JsonObject document, final Collection<String> result) throws IllegalArgumentException {
	
	for (final Entry<String, JsonValue> entry : document.entrySet()) {

	    if ("@context".equals(entry.getKey())) {
		processContextValue(entry.getValue(), result);
		
	    } else if (ValueType.OBJECT.equals(entry.getValue().getValueType())) {
		getReferencedContexts(entry.getValue().asJsonObject(), result);
	    }
	}
	
	return result;
    }
    
    static final void processContextValue(final JsonValue value, final Collection<String> result) {
    
	switch (value.getValueType()) {
	case STRING:
	    final String uri = ((JsonString)value).getString();
	    
	    if (isAbsoluteURI(uri)) {
		result.add(uri);
		break;
	    }		    
	    
	case ARRAY:
	    value.asJsonArray().forEach(item -> processContextValue(item, result));
	    break;
	    
	case OBJECT:
	    if (value.asJsonObject().size() == 1&& value.asJsonObject().containsKey("@id")) {
		
		final JsonValue id = value.asJsonObject().get("@id");
		
		if (ValueType.STRING.equals(id.getValueType())) {
		    
		    final String idUri = ((JsonString)id).getString();
		    
		    if (isAbsoluteURI(idUri)) {
			result.add(idUri);
			break;
		    }		    
		}
	    }
	    
	default:
	    throw new IllegalArgumentException("Non serializable context detected.");
	}
    }
    
    static final boolean isAbsoluteURI(String uri) {
	try {
	    
	    return URI.create(uri).isAbsolute();
	    
	} catch (IllegalArgumentException e) {
	    
	}
	return false;
    }

}
