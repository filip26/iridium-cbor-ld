package com.apicatalog.cborld;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URI;
import java.util.Collection;
import java.util.HashSet;

import com.apicatalog.json.cursor.JsonBuilder;
import com.apicatalog.json.cursor.JsonCursor;
import com.apicatalog.json.cursor.JsonObjectCursor;

public final class CborLd {

    
    public static final byte[] encode(JsonObjectCursor document) {
	
	if (document == null) {
	    throw new IllegalArgumentException("The 'document' parameter must not be null.");
	}
	
	try {
	    
	    final Collection<String> contexts = getReferencedContexts(document, new HashSet<>());
	    
	    return compress(document, contexts);
	    
	} catch (IOException e) {
	    e.printStackTrace();
	    
	} catch (IllegalArgumentException e) {
	    e.printStackTrace();
	    
	    // non compressable context
	}
	
	return null;
    }
    
    public static final JsonBuilder decode(byte[] encodedDocument) {
	return null;
    }
    
    
    /**
     * Compresses the given JSON-LD document into CBOR-LD byte array.
     * 
     * @see <a href="https://digitalbazaar.github.io/cbor-ld-spec/#compressed-cbor-ld-buffer-algorithm">Compressed CBOR-LD Buffer Algorithm</a>
     * 
     * @param document the document to compress
     * @param contextUrls a set of URLs of <code>@context</code> referenced by the document 
     * @return the compressed document as byte array
     * 
     * @throws IOException
     */
    static final byte[] compress(final JsonObjectCursor document, Collection<String> contextUrls) throws IOException {
	
	// 1.
	final ByteArrayOutputStream result = new ByteArrayOutputStream();
	
	// 2.CBOR Tag - 0xD9, CBOR-LD - 0x50, Compressed - CBOR-LD compression algorithm version 1 - 0x01
	result.write(new byte[] { (byte) 0xD9, 0x50, 0x01} );
	
	// 3.
	
	// 4.
	
	
	// 5.
	return result.toByteArray();
    }
    
    static final Dictionary getTermMap(Collection<String> contextUrls) {
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
