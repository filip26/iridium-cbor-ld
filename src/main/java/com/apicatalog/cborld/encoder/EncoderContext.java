package com.apicatalog.cborld.encoder;

import java.util.Collection;
import java.util.LinkedHashSet;

import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.uri.UriUtils;

public class EncoderContext {

    public final static Collection<String> get(final MapCursor document) {
        return get(document, new LinkedHashSet<>());
    }

    static final Collection<String> get(final MapCursor document, Collection<String> contexts) {
        for (final String property : document.keys()) {
    
            if (Keywords.CONTEXT.equals(property)) {
                processContextValue(document.value(property), contexts);
                document.parent();
    
            } else if (document.isObject(property)) {
                get(document.object(property), contexts);
                document.parent();
            }
        }
    
        return contexts;
    }

    static final void processContextValue(final ValueCursor value, final Collection<String> result) {
    
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
    
            if (value.asObject().size() == 1 && value.asObject().isString(Keywords.ID)) {
    
                final String id = value.asObject().stringValue(Keywords.ID);
        
                if (UriUtils.isAbsoluteUri(id, true)) {
                    result.add(id);
                    return;
                }
            }
        }
    
        throw new IllegalArgumentException("Non serializable context detected.");
    }
}
