package com.apicatalog.cborld.encoder;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map.Entry;

import com.apicatalog.cborld.encoder.EncoderException.Code;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.jsonld.uri.UriValidationPolicy;
import com.apicatalog.tree.io.NodeAdapter;

import jakarta.json.JsonString;

class EncoderContext {

    public static final Collection<String> get(final Object document, final NodeAdapter adapter) throws EncoderException {
        return get(document, adapter, new LinkedHashSet<>());
    }

    static final Collection<String> get(final Object document, final NodeAdapter adapter, Collection<String> contexts) throws EncoderException {

        for (final Entry<?, ?> entry : adapter.entries(document)) {

            if (Keywords.CONTEXT.equals(entry.getKey())) {
                processContextValue(entry.getValue(), adapter, contexts);

            } else if (adapter.isMap(entry.getValue())) {
                get(entry.getValue(), adapter, contexts);
            }
        }
        return contexts;
    }

    static final void processContextValue(final Object jsonValue, NodeAdapter adapter, final Collection<String> result) throws EncoderException {

        if (adapter.isString(jsonValue)) {
            final String uri = ((JsonString) jsonValue).getString();

            if (UriUtils.isAbsoluteUri(uri, UriValidationPolicy.Full)) {
                result.add(uri);
                return;
            }

        } else if (adapter.isCollection(jsonValue) && !adapter.isEmpty(jsonValue)) {

            for (final Object item : adapter.elements(jsonValue)) {
                processContextValue(item, adapter, result);
            }
            return;

        } else if (adapter.isMap(jsonValue)) {

            var idNode = adapter.property(Keywords.ID, jsonValue);
            
            if (adapter.size(jsonValue) == 1 && adapter.isString(idNode)) {

                final String id = adapter.stringValue(idNode);

                if (UriUtils.isAbsoluteUri(id, UriValidationPolicy.Full)) {
                    result.add(id);
                    return;
                }
            }
        }

        throw new EncoderException(
                Code.NonCompressible,
                """
                        Non-compressible document. Only JSON-LD documents containing referenced contexts can be compressed. \
                        Referenced contexts serve as a shared dictionary, which is not possible with inline contexts.
                        """);
    }
}
