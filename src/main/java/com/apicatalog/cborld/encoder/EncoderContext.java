package com.apicatalog.cborld.encoder;

import java.util.Collection;
import java.util.LinkedHashSet;

import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.cursor.ArrayItemCursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.MapEntryCursor;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.uri.UriUtils;

public class EncoderContext {

    public final static Collection<String> get(final MapCursor document) {
        return get(document, new LinkedHashSet<>());
    }

    static final Collection<String> get(final MapCursor document, Collection<String> contexts) {

        for (final MapEntryCursor entry : document) {
    
            if (Keywords.CONTEXT.equals(entry.mapKey())) {
                processContextValue(entry, contexts);

            } else if (entry.isMap()) {
                get(entry.asMap(), contexts);
            }
        }
        document.parent();    
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
    
            for (final ArrayItemCursor item : value.asArray()) {
                processContextValue(item, result);                
            }            
            value.parent();
            return;
    
        } else if (value.isMap()) {
    
            if (value.asMap().size() == 1 && value.asMap().isString(Keywords.ID)) {
    
                final String id = value.asMap().stringValue(Keywords.ID);
        
                if (UriUtils.isAbsoluteUri(id, true)) {
                    result.add(id);
                    return;
                }
            }
        }
    
        throw new IllegalArgumentException("Non compress-able context detected.");
    }
}
