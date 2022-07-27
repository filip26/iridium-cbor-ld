package com.apicatalog.cborld.context;

import java.math.BigInteger;
import java.util.Collection;
import java.util.LinkedHashSet;

import com.apicatalog.cborld.Hex;
import com.apicatalog.cborld.context.ContextError.Code;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.dictionary.KeywordDictionary;
import com.apicatalog.json.cursor.JsonCursor;
import com.apicatalog.json.cursor.JsonObjectCursor;
import com.apicatalog.jsonld.uri.UriUtils;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

public class Context {

    private final Dictionary dictionary;
    
    public Context(final Dictionary dictionary) {
	this.dictionary = dictionary;
    }

    public final Collection<String> get(DataItem data) throws ContextError {
	return get(data, new LinkedHashSet<>());
    }
    
    public final Collection<String> get(final JsonObjectCursor document) {
	return get(document, new LinkedHashSet<>());
    }
    
    static final Collection<String> get(final JsonObjectCursor document, Collection<String> contexts) {
	for (final String property : document.properies()) {

	    if ("@context".equals(property)) {
		processContextValue(document.value(property), contexts);
		document.parent();

	    } else if (document.isObject(property)) {
		get(document.object(property), contexts);
		document.parent();
	    }
	}

	return contexts;	
    }
    
    static final void processContextValue(final JsonCursor value, final Collection<String> result) {

	if (value.isString()) {
	    final String uri = value.stringValue();

	    if (UriUtils.isAbsoluteUri(uri, true)) {
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

	    if (value.asObject().size() == 1 && value.asObject().isString("@id")) {

		final String id = value.asObject().stringValue("@id");

		if (UriUtils.isAbsoluteUri(id, true)) {
		    result.add(id);
		    return;
		}
	    }
	}

	throw new IllegalArgumentException("Non serializable context detected.");
    }

    final Collection<String> get(DataItem data, Collection<String> contexts) throws ContextError {
	
	if (data == null) {
	    throw new IllegalArgumentException("The data parameter must not be null.");
	}

	switch (data.getMajorType()) {
	case MAP:
	    return get((Map) data, contexts);
	
	case ARRAY:
	    return get(((Array) data).getDataItems(), contexts);
	    
	default:
	    System.out.println("TODO: " + data.getMajorType());
	    break;
	}
	
	return contexts;
    }

    final Collection<String> get(final Map map, Collection<String> contexts) throws ContextError {
	
	if (map == null) {
	    throw new IllegalArgumentException("The map parameter must not be null.");
	}
	
	
	for (final DataItem key : map.getKeys()) {
	    
	    if (isContextKey(key)) {
	

		final DataItem value = map.get(key);
		
		String contextUrl = null;
		
		switch (value.getMajorType()) {
		case UNICODE_STRING:
		    contextUrl = ((UnicodeString)value).getString();
		    break;
		    
		case ARRAY:
		    //TODO
		    System.out.println("TODO: " + value.getMajorType());
		    break;
		    
		case UNSIGNED_INTEGER:
		    
		    final BigInteger unsigned = ((UnsignedInteger)value).getValue();
		    
		    contextUrl = dictionary.getValue(unsigned);
		    
		    if (contextUrl == null) {
			throw new ContextError(Code.UnknownContextCode, "The code [" + Hex.toString(unsigned.toByteArray()) + "]. Cannot get the context.");
		    }

		    break;
		    
		default:
		    System.out.println("TODO: " + value.getMajorType());
		    break;
		}
		
		if (contextUrl != null) {
		    contexts.add(contextUrl);
		}
	    }
	}
	
	return contexts;
    }

    final Collection<String> get(final Collection<DataItem> items, Collection<String> contexts) throws ContextError {
	
	if (items == null) {
	    throw new IllegalArgumentException("The items parameter must not be null.");
	}
	
	for (final DataItem item : items) {
	    contexts = get(item, contexts);
	}
	
	return contexts;
    }
    
    final static boolean isContextKey(final DataItem data) throws ContextError {
	
	if (data == null) {
	    throw new IllegalArgumentException("The data parameter must not be null.");
	}

	byte[] code = null;
	
	switch (data.getMajorType()) {
	case UNICODE_STRING:
	    code = ((UnicodeString)data).getString().getBytes();
	    break;
	    
	case UNSIGNED_INTEGER:
	    code = ((UnsignedInteger)data).getValue().toByteArray();
	    break;
	    
	default:
    	    throw new ContextError(Code.Unsupported, "A property name of type [" + data.getMajorType() +"] is not supported.");
	}
	
	return code != null && code.length == 1 && KeywordDictionary.CONTEXT_CODE == code[0]; 
    }
}
