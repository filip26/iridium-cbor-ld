package com.apicatalog.cborld;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;

import com.apicatalog.json.cursor.JsonBuilder;
import com.apicatalog.json.cursor.JsonCursor;
import com.apicatalog.json.cursor.JsonObjectCursor;

public final class CborLd {

    
    public static final byte[] encode(JsonObjectCursor document) {
	
	try {
	    
	    final Collection<String> contexts = getReferencedContexts(document, new HashSet<>());
	    
	    
	    
	} catch (IllegalArgumentException e) {
	    
	    // non compressable context
	}
	
	return null;
    }
    
    public static final JsonBuilder decode(byte[] encodedDocument) {
	return null;
    }
    
    
    static final Collection<String> getReferencedContexts(final JsonObjectCursor document, final Collection<String> result) throws IllegalArgumentException {

	for (final String property : document.properies()) {

	    if ("@context".equals(property)) {
		processContextValue(document.value(property), result);
		document.parent();
	    
	    } else if (document.isObject(property)) {
		getReferencedContexts(document.object(property), result);
		document.parent();
	    }
	}
	
	return result;
    }
    
    static final void processContextValue(final JsonCursor value, final Collection<String> result) {
    
	if (value.isString()) {
	    final String uri = value.stringValue();
	    
	    if (isAbsoluteURI(uri)) {
		result.add(uri);
		return;
	    }
	    
	} else if (value.isNonEmptyArray()) {

	    for (int i = 0; i < value.asArray().size(); i++) {
		processContextValue(value.value(i), result);
		value.parent();
	    }
	    return;

	} else if (value.isObject()) {

	    if (value.asObject().size() == 1&& value.asObject().isString("@id")) {
		
		final String id = value.asObject().stringValue("@id");

		if (isAbsoluteURI(id)) {
		    result.add(id);
		    return;
		}		    
	    }	    
	}

	throw new IllegalArgumentException("Non serializable context detected.");
    }
    
    static final boolean isAbsoluteURI(String uri) {
	try {
	    
	    return URI.create(uri).isAbsolute();
	    
	} catch (IllegalArgumentException e) {
	    
	}
	return false;
    }

}
