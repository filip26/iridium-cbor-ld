package com.apicatalog.cborld.dictionary;

import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

public class CodecTermMap2 {

    final Map<Integer, String> index;
    final Map<String, Integer> reverse;
    
    final Collection<String> types;

    int lastCustomIndex;

    protected CodecTermMap2(Map<Integer, String> index, Collection<String> types, int lastCustomIndex) {
	this.index = index;
	this.reverse = index
			.entrySet()
		       	.stream()
		       	.collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey));	

	this.types = types;
	
	this.lastCustomIndex = lastCustomIndex;
    }

    public static CodecTermMap2 from(Collection<String> contextUrls, DocumentLoader loader) {

	Map<String, String> result = new HashMap<>();
	Set<String> types = new HashSet<>();

	try {
	    for (final String contextUrl : contextUrls) {

		Document document = loader.loadDocument(URI.create(contextUrl), new DocumentLoaderOptions());

		if (document == null) {
		    // TODO warning
		    continue;
		}
		if (!document.getJsonContent().isPresent()) {
		    // TODO warning
		    continue;
		}

		JsonStructure context = document.getJsonContent().get();

		if (JsonUtils.isNonEmptyArray(context)) {
		    process(context.asJsonArray(), result, types);

		} else if (JsonUtils.isNonEmptyObject(context)) {
		    process(context.asJsonObject(), result, types);

		} else {
		    // TODO
		}

	    }

	} catch (JsonLdError e) {
	    // TODO Auto-generated catch block
	    e.printStackTrace();
	}

	final CodecTermMap2 map = 
		new CodecTermMap2(
			new LinkedHashMap<>(KeywordDictionary.CODE_TO_TERM),
			types,
			KeywordDictionary.CUSTOM_OFFSET
		);

	result.keySet().stream().sorted().forEach(map::add);

	return map;
    }

    void add(String key) {
	index.put(lastCustomIndex, key);
	reverse.put(key,  lastCustomIndex);
	lastCustomIndex += 2;
    }

    final static void process(JsonObject object, Map<String, String> index, Set<String> types) {

	for (final Map.Entry<String, JsonValue> entry : object.entrySet()) {

	    if (!Keywords.contains(entry.getKey())) {
		index.put(entry.getKey(), null); // TODO value
		
		if (JsonUtils.isString(entry.getValue())
			&& Keywords.TYPE.equals(((JsonString)entry.getValue()).getString())) {
		    types.add(entry.getKey());
		}

	    }
	    
	    if (JsonUtils.isObject(entry.getValue())) {
		process(entry.getValue().asJsonObject(), index, types);
		continue;
	    }

	}

    }

    final static void process(JsonArray array, Map<String, String> index, Set<String> types) {

	for (final JsonValue jsonValue : array) {
	    if (JsonUtils.isObject(jsonValue)) {
		process(jsonValue.asJsonObject(), index, types);

	    } else if (JsonUtils.isString(jsonValue)) {
		// TODO
	    }
	}

    }

    final static void process(final JsonValue value, final Map<String, String> index, Set<String> types) {

	if (JsonUtils.isObject(value)) {

	}

    }

    public String getTerm(int code) {
	return index.get(code);
    }
    
    public Integer getCode(String term) {
	return reverse.get(term);
    }

    public boolean isType(String term) {
	return types.contains(term);
    }
}
