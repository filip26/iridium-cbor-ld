package com.apicatalog.cborld.encoder.value;

import com.apicatalog.cborld.dictionary.CodecTermMap;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.json.cursor.JsonValueCursor;
import com.apicatalog.jsonld.context.TermDefinition;

import co.nstant.in.cbor.model.DataItem;

public class IdValueEncoder implements ValueEncoder {

    @Override
    public DataItem encode(Dictionary dictionary, JsonValueCursor value, String term, TermDefinition def) {
	// TODO Auto-generated method stub
	return null;
    }

    /*

	    System.out.println(">>> " + value);
	    System.out.println("  > " + def);
	    if (def != null) {	    
        	    if (Keywords.CONTEXT.equals(def.getUriMapping())) {	    
        		final byte[] code = contexts.getCode(value.stringValue());
        		
        		if (code != null) {
        		    return new UnsignedInteger(new BigInteger(code));
        		}
        		
        	    } else if (Keywords.TYPE.equals(def.getUriMapping())) {
        		final Integer code = index.getCode(value.stringValue());
        				    
        		if (code != null) {
        		    return new UnsignedInteger(code);
        		}		    
        		    
        	    } else if (Keywords.ID.equals(def.getUriMapping())) {
        		final Integer code = index.getCode(value.stringValue());
        				    
        		if (code != null) {
        		    return new UnsignedInteger(code);
        		} 
        	    }


     */
    
}
