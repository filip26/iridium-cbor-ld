package com.apicatalog.cborld.encoder;

import java.util.Collection;
import java.util.LinkedHashSet;

import com.apicatalog.json.cursor.JsonObjectCursor;
import com.apicatalog.json.cursor.JsonValueCursor;
import com.apicatalog.jsonld.uri.UriUtils;

public class EncoderContext {

    public final static Collection<String> get(final JsonObjectCursor document) {
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
    
    static final void processContextValue(final JsonValueCursor value, final Collection<String> result) {

	if (value.isString()) {
	    final String uri = value.stringValue();

	    if (UriUtils.isAbsoluteUri(uri, true)) {
		result.add(uri);
		return;
	    }

	} else if (value.isNonEmptyArray()) {

	    for (int i = 0; i < value.asArray().size(); i++) {
		processContextValue(value.asArray().value(i), result);
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
}
