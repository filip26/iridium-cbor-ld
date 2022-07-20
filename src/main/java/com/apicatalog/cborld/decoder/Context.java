package com.apicatalog.cborld.decoder;

import java.util.Collection;
import java.util.LinkedHashSet;

import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.dictionary.KeywordDictionary;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

public class Context {


    protected Context() {
    }

    public static final Collection<String> get(DataItem data) throws DecoderError {
	return get(data, new LinkedHashSet<>());
    }

    static final Collection<String> get(DataItem data, Collection<String> contexts) throws DecoderError {
	
	if (data == null) {
	    throw new IllegalArgumentException("The data parameter must not be null.");
	}

	switch (data.getMajorType()) {
	case MAP:
	    return get((Map) data, contexts);
	
	case ARRAY:
	    return get(((Array) data).getDataItems(), contexts);
	    
	default:
	    break;
	}
	
	return contexts;
    }

    final static Collection<String> get(final Map map, Collection<String> contexts) throws DecoderError {
	
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
		    
		default:
		    break;
		}
		
		if (contextUrl != null) {
		    contexts.add(contextUrl);
		}
	    }
	}
	
	return contexts;
    }

    final static Collection<String> get(final Collection<DataItem> items, Collection<String> contexts) throws DecoderError {
	
	if (items == null) {
	    throw new IllegalArgumentException("The items parameter must not be null.");
	}
	
	for (final DataItem item : items) {
	    contexts = get(item, contexts);
	}
	
	return contexts;
    }
    
    final static boolean isContextKey(final DataItem data) throws DecoderError {
	
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
    	    throw new DecoderError(Code.Unsupported, "A property name of type [" + data.getMajorType() +"] is not supported.");
	}
	
	return code != null && code.length == 1 && KeywordDictionary.CONTEXT_CODE == code[0]; 
    }
}
