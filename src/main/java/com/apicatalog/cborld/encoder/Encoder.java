package com.apicatalog.cborld.encoder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.context.Context;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.dictionary.CodecTermMap;
import com.apicatalog.cborld.dictionary.ContextDictionary;
import com.apicatalog.cborld.encoder.EncoderError.Code;
import com.apicatalog.cborld.encoder.value.ContextValueEncoder;
import com.apicatalog.cborld.encoder.value.IdValueEncoder;
import com.apicatalog.cborld.encoder.value.TypeValueEncoder;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.json.cursor.JsonArrayCursor;
import com.apicatalog.json.cursor.JsonObjectCursor;
import com.apicatalog.json.cursor.JsonValueCursor;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.CborBuilder;
import co.nstant.in.cbor.CborEncoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.builder.ArrayBuilder;
import co.nstant.in.cbor.builder.MapBuilder;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.SimpleValue;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

public class Encoder {

    protected final JsonObjectCursor document;
    protected final DocumentLoader loader;
    
    protected CodecTermMap index;
    
    protected final Collection<ValueEncoder> valueEncoders;
    
    // options
    protected boolean compactArrays;
    
    protected Encoder(JsonObjectCursor document, DocumentLoader loader) {
	this.document = document;
	this.loader = loader;
	
	this.valueEncoders = new ArrayList<>();	//FIXME
	
	valueEncoders.add(new ContextValueEncoder());
	valueEncoders.add(new IdValueEncoder());
	valueEncoders.add(new TypeValueEncoder());
	
	// default options
	this.compactArrays = true;
    }

    public static final Encoder create(JsonObjectCursor document, DocumentLoader loader) throws EncoderError {

	if (document == null) {
	    throw new IllegalArgumentException("The 'document' parameter must not be null.");
	}
	
	return new Encoder(document, loader);
    }
    
    /**
     * If set to true, the encoder replaces arrays with
     * just one element with that element during encoding saving one byte.
     * Enabled by default.
     * 
     * @param enable <code>true</code> to enable arrays compaction
     * @return {@link Encoder} instance
     * 
     */
    public Encoder compactArray(boolean enable) {
	compactArrays = enable;
	return this;
    }
    
    public byte[] encode() throws EncoderError, ContextError {

	try {
	    
	    final Collection<String> contexts = new Context(new ContextDictionary()).get(document);	//FIXME dictionary

	    if (contexts.isEmpty()) { // is not JSON-LD document
		throw new EncoderError(Code.InvalidDocument, "Not a valid JSON-LD document in a compacted form.");
	    }
	    
	    return compress(document, contexts);

	} catch (IOException e) {
	    e.printStackTrace();

	    // non compressable context
	} catch (IllegalArgumentException e) {
	    e.printStackTrace();

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
     * @throws ContextError 
     */
    final byte[] compress(final JsonObjectCursor document, Collection<String> contextUrls) throws IOException, ContextError {

	// 1.
	final ByteArrayOutputStream result = new ByteArrayOutputStream();

	// 2.CBOR Tag - 0xD9, CBOR-LD - 0x05, Compressed - CBOR-LD compression algorithm
	// version 1 - 0x01
	result.write(CborLd.CBOR_LD_BYTE_PREFIX);
	result.write(CborLd.COMPRESSED);

	index = CodecTermMap.from(contextUrls, loader);
	
	return toCbor(document, result);	
    }

    final byte[] toCbor(JsonObjectCursor object, ByteArrayOutputStream baos) {

	try {
	    final CborBuilder builder = (CborBuilder) toCbor(object, new CborBuilder().addMap(), null).end();

	    new CborEncoder(baos).encode(builder.build());

	} catch (CborException e) {
	    e.printStackTrace();
	}

	return baos.toByteArray();
    }

    final MapBuilder<?> toCbor(final JsonObjectCursor object, final MapBuilder<?> builder, TermDefinition def) {
	
	MapBuilder<?> flow = builder;

	for (final String property : object.properies()) {

	    final Integer encodedProperty = index.getCode(property);

	    if (object.isArray(property)) {	

		final DataItem key = encodedProperty != null 
	    			? new UnsignedInteger(encodedProperty + 1)
	    			: new UnicodeString(property);

		object.array(property);
		
		if (compactArrays && object.asArray().size() == 1) {

		    object.asArray().value(0);
		    
		    if (object.isObject()) {
			flow = (MapBuilder<?>) toCbor(object.asObject(), flow.putMap(key), null).end();
			
		    } else if (object.isArray()) {
			flow = (MapBuilder<?>) toCbor(object.asArray(), flow.putArray(key), null).end();
			
		    } else {
			final DataItem value = toCbor(object, property, index.getDefinition(def, property));
			    
			flow = flow.put(key, value);
		    }

		    object.parent();
			    
		} else {
		    flow = (MapBuilder<?>) toCbor(object.asArray(), flow.putArray(key), 
    			index.getDefinition(def, property)
    			).end();
		}
		
		object.parent();
		continue;
	    }
	    
	    final DataItem key = encodedProperty != null 
			? new UnsignedInteger(encodedProperty)
			: new UnicodeString(property);

	    if (object.isObject(property)) {

		flow = (MapBuilder<?>) toCbor(object.object(property), flow.putMap(key),
			index.getDefinition(def, property)
			).end();
		
		object.parent();
		continue;
	    } 
	    
	    final DataItem value = toCbor(object.value(property), property, index.getDefinition(def, property));
	    
	    flow = flow.put(key, value);
	    
	    object.parent();
	}
	return flow;
    }
    
    final DataItem toCbor(final JsonValueCursor value, final String term, final TermDefinition def) {
	
	if (value.isBoolean()) {
	    return value.booleanValue() ? SimpleValue.TRUE : SimpleValue.FALSE;
	}
			
	if (value.isString()) {
	    
	    //TODO better
	    for (final ValueEncoder valueEncoder : valueEncoders) {
		DataItem dataItem = valueEncoder.encode(index, value, term, def);
		if (dataItem != null) {
		    return dataItem;
		}
	    }
	    return new UnicodeString(value.stringValue());
	}
	
	if (value.isNumber()) {
	    //TODO
	    return new UnsignedInteger(value.integerValue());
	}
	
	throw new IllegalStateException("TODO " + value);
    }

    final ArrayBuilder<?> toCbor(final JsonArrayCursor object, final ArrayBuilder<?> builder, TermDefinition def) {

	ArrayBuilder<?> flow = builder;

	for (int i = 0; i < object.size(); i++) {

	    if (object.isObject(i)) {
		flow = (ArrayBuilder<?>) toCbor(object.object(i), flow.startMap(), def).end();
		object.parent();
		continue;
	    } 
	    
	    if (object.isArray(i)) {		
		flow = (ArrayBuilder<?>) toCbor(object.array(i), flow.startArray(), def).end();
		object.parent();
		continue;
	    }
	    
	    DataItem value = toCbor(object.value(i), (String)null, def);
	    object.parent();
		
	    flow = flow.add(value);		
	}
	return flow;
    }
}
