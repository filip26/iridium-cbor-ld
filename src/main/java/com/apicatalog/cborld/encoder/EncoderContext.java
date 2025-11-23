package com.apicatalog.cborld.encoder;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map.Entry;

import com.apicatalog.cborld.encoder.EncoderException.EncoderError;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.web.uri.UriUtils;
import com.apicatalog.web.uri.UriValidationPolicy;

class EncoderContext {

    public static final Collection<String> get(final Object document, final TreeAdapter adapter) throws EncoderException {
        return get(document, adapter, new LinkedHashSet<>());
    }

    static final Collection<String> get(final Object document, final TreeAdapter adapter, Collection<String> contexts) throws EncoderException {

        for (final Entry<?, ?> entry : adapter.entries(document)) {

            if (Keywords.CONTEXT.equals(entry.getKey())) {
                processContextValue(entry.getValue(), adapter, contexts);

            } else if (adapter.isMap(entry.getValue())) {
                get(entry.getValue(), adapter, contexts);
            }
        }
        return contexts;
    }

    static final void processContextValue(final Object jsonValue, TreeAdapter adapter, final Collection<String> result) throws EncoderException {

        if (adapter.isString(jsonValue)) {
            final String uri = adapter.stringValue(jsonValue);

            if (UriUtils.isAbsoluteUri(uri, UriValidationPolicy.Full)) {
                result.add(uri);
                return;
            }

        } else if (adapter.isCollection(jsonValue) && !adapter.isEmpty(jsonValue)) {

            for (final Object item : adapter.elements(jsonValue)) {
                processContextValue(item, adapter, result);
            }
            return;

        } else if (adapter.isMap(jsonValue) && adapter.isSingleEntry(jsonValue)) {

            var idNode = adapter.property(Keywords.ID, jsonValue);

            if (adapter.isString(idNode)) {

                final String id = adapter.stringValue(idNode);

                if (UriUtils.isAbsoluteUri(id, UriValidationPolicy.Full)) {
                    result.add(id);
                    return;
                }
            }
        }

        throw new EncoderException(
                EncoderError.NON_COMPRESSIBLE,
                """
                        Non-compressible document. Only JSON-LD documents containing referenced contexts can be compressed. \
                        Referenced contexts serve as a shared dictionary, which is not possible with inline contexts.
                        """);
    }
}
