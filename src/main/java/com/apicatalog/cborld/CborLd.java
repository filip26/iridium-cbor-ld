package com.apicatalog.cborld;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URI;
import java.util.Collection;
import java.util.HashSet;

import com.apicatalog.json.cursor.JsonArrayCursor;
import com.apicatalog.json.cursor.JsonBuilder;
import com.apicatalog.json.cursor.JsonCursor;
import com.apicatalog.json.cursor.JsonObjectCursor;

import co.nstant.in.cbor.CborBuilder;
import co.nstant.in.cbor.CborEncoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.builder.ArrayBuilder;
import co.nstant.in.cbor.builder.MapBuilder;

public final class CborLd {

    public static final byte[] encode(JsonObjectCursor document) {

	if (document == null) {
	    throw new IllegalArgumentException("The 'document' parameter must not be null.");
	}

	try {

	    final Collection<String> contexts = getReferencedContexts(document, new HashSet<>());

	    if (contexts.isEmpty()) { // is not JSON-LD document
		//TODO log warning 
		return toCbor(document);
	    }

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
     * @see <a href=
     *      "https://digitalbazaar.github.io/cbor-ld-spec/#compressed-cbor-ld-buffer-algorithm">Compressed
     *      CBOR-LD Buffer Algorithm</a>
     * 
     * @param document    the document to compress
     * @param contextUrls a set of URLs of <code>@context</code> referenced by the
     *                    document
     * @return the compressed document as byte array
     * 
     * @throws IOException
     */
    static final byte[] compress(final JsonObjectCursor document, Collection<String> contextUrls) throws IOException {

	// 1.
	final ByteArrayOutputStream result = new ByteArrayOutputStream();

	// 2.CBOR Tag - 0xD9, CBOR-LD - 0x50, Compressed - CBOR-LD compression algorithm
	// version 1 - 0x01
	result.write(new byte[] { (byte) 0xD9, 0x05, 0x01 });

	// 3.

	// 4.

	// 5.
	return result.toByteArray();
    }

    static final Dictionary getTermMap(Collection<String> contextUrls) {
	return null;
    }

    static final Collection<String> getReferencedContexts(final JsonObjectCursor document,
	    final Collection<String> result) throws IllegalArgumentException {

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

	    if (value.asObject().size() == 1 && value.asObject().isString("@id")) {

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

    static final byte[] toCbor(JsonObjectCursor object) {

	final ByteArrayOutputStream baos = new ByteArrayOutputStream();

	try {
	    final CborBuilder builder = (CborBuilder) toCbor(object, new CborBuilder().addMap()).end();

	    new CborEncoder(baos).encode(builder.build());

	} catch (CborException e) {
	    e.printStackTrace();
	}

	return baos.toByteArray();
    }

    static final MapBuilder<?> toCbor(final JsonObjectCursor object, final MapBuilder<?> builder) {
	
	MapBuilder<?> flow = builder;

	for (final String property : object.properies()) {

	    if (object.isObject(property)) {
		flow = (MapBuilder<?>) toCbor(object.object(property), flow.putMap(property)).end();

	    } else if (object.isArray(property)) {		
		flow = (MapBuilder<?>) toCbor(object.array(property), flow.putArray(property)).end();

	    } else if (object.isBoolean(property)) {
		flow = flow.put(property, object.booleanValue(property));
		
	    } else if (object.isString(property)) {
		flow = flow.put(property, object.stringValue(property));
		
	    } else if (object.isNumber(property)) {
		//TODO
	    }
	}
	return flow;
    }

    static final ArrayBuilder<?> toCbor(final JsonArrayCursor object, final ArrayBuilder<?> builder) {
	
	ArrayBuilder<?> flow = builder;

	for (int i = 0; i < object.size(); i++) {

	    if (object.isObject(i)) {
		flow = (ArrayBuilder<?>) toCbor(object.object(i), flow.startMap()).end();

	    } else if (object.isArray(i)) {		
		flow = (ArrayBuilder<?>) toCbor(object.array(i), flow.startArray()).end();

	    } else if (object.isBoolean(i)) {
		flow = flow.add(object.booleanValue(i));
		
	    } else if (object.isString(i)) {
		flow = flow.add(object.stringValue(i));
		
	    } else if (object.isNumber(i)) {
		//TODO
	    }
	}
	return flow;
    }
}
