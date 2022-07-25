package com.apicatalog.cborld.dictionary;

import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

public class CodecTermMap {

    final Map<Integer, String> index;
    final Map<String, Integer> reverse;

    int lastCustomIndex;

    protected CodecTermMap(Map<Integer, String> index, int lastCustomIndex) {
	this.index = index;
	this.reverse = index
			.entrySet()
		       	.stream()
		       	.collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey));	

	this.lastCustomIndex = lastCustomIndex;
    }

    public static CodecTermMap from(Collection<String> contextUrls, DocumentLoader loader) {

	Map<String, String> result = new HashMap<>();

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
		    process(context.asJsonArray(), result);

		} else if (JsonUtils.isNonEmptyObject(context)) {
		    process(context.asJsonObject(), result);

		} else {
		    // TODO
		}

	    }

	} catch (JsonLdError e) {
	    // TODO Auto-generated catch block
	    e.printStackTrace();
	}

	final CodecTermMap map = new CodecTermMap(new LinkedHashMap<>(KeywordDictionary.CODE_TO_TERM),
		KeywordDictionary.CUSTOM_OFFSET);

	result.keySet().stream().sorted().forEach(map::add);

	return map;
    }

    void add(String key) {
	index.put(lastCustomIndex, key);
	reverse.put(key,  lastCustomIndex);
	lastCustomIndex += 2;
    }

    final static void process(JsonObject object, Map<String, String> index) {

	for (final Map.Entry<String, JsonValue> entry : object.entrySet()) {

	    if (!Keywords.contains(entry.getKey())) {
		index.put(entry.getKey(), null); // TODO value
	    }
	    
	    if (JsonUtils.isObject(entry.getValue())) {
		process(entry.getValue().asJsonObject(), index);
		continue;
	    }

	}

    }

    final static void process(JsonArray array, Map<String, String> index) {

	for (final JsonValue jsonValue : array) {
	    if (JsonUtils.isObject(jsonValue)) {
		process(jsonValue.asJsonObject(), index);

	    } else if (JsonUtils.isString(jsonValue)) {
		// TODO
	    }
	}

    }

    final static void process(final JsonValue value, final Map<String, String> index) {

	if (JsonUtils.isObject(value)) {

	}

    }

    public String getTerm(int code) {
	return index.get(code);
    }
    
    public Integer getCode(String term) {
	return reverse.get(term);
    }

}
