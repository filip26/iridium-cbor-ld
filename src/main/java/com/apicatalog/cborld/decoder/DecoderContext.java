package com.apicatalog.cborld.decoder;

import java.math.BigInteger;
import java.util.Collection;
import java.util.LinkedHashSet;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.context.ContextError.Code;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.dictionary.KeywordDictionary;
import com.apicatalog.hex.Hex;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

public class DecoderContext {

    private final Dictionary dictionary;
    
    public DecoderContext(final Dictionary dictionary) {
	this.dictionary = dictionary;
    }

    public final Collection<String> get(DataItem data) throws ContextError {
	return get(data, new LinkedHashSet<>());
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
