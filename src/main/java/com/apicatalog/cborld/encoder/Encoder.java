package com.apicatalog.cborld.encoder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collection;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.context.Context;
import com.apicatalog.cborld.dictionary.CodecTermMap;
import com.apicatalog.cborld.dictionary.ContextDictionary;
import com.apicatalog.cborld.encoder.EncoderError.Code;
import com.apicatalog.json.cursor.JsonArrayCursor;
import com.apicatalog.json.cursor.JsonObjectCursor;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.CborBuilder;
import co.nstant.in.cbor.CborEncoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.builder.ArrayBuilder;
import co.nstant.in.cbor.builder.MapBuilder;

public class Encoder {

    protected final JsonObjectCursor document;
    protected final DocumentLoader loader;
    
    protected CodecTermMap index;
    
    protected Encoder(JsonObjectCursor document, DocumentLoader loader) {
	this.document = document;
	this.loader = loader;
    }

    public static final Encoder create(JsonObjectCursor document, DocumentLoader loader) throws EncoderError {

	if (document == null) {
	    throw new IllegalArgumentException("The 'document' parameter must not be null.");
	}
	
	return new Encoder(document, loader);
    }
    
    public byte[] encode() throws EncoderError {

	try {

	    final Collection<String> contexts = new Context(new ContextDictionary()).get(document);	//FIXME dictionary

	    if (contexts.isEmpty()) { // is not JSON-LD document
		throw new EncoderError(Code.InvalidDocument, "Not a valid JSON-LD document in a compacted form.");
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
    final byte[] compress(final JsonObjectCursor document, Collection<String> contextUrls) throws IOException {

	// 1.
	final ByteArrayOutputStream result = new ByteArrayOutputStream();

	// 2.CBOR Tag - 0xD9, CBOR-LD - 0x05, Compressed - CBOR-LD compression algorithm
	// version 1 - 0x01
	result.write(CborLd.CBOR_LD_BYTE_PREFIX);
	result.write(CborLd.COMPRESSED);

	index = CodecTermMap.from(contextUrls, loader);
	
	return toCbor(document, result);	
    }

    static final byte[] toCbor(JsonObjectCursor object, ByteArrayOutputStream baos) {

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
