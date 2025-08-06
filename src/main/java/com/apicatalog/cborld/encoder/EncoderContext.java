package com.apicatalog.cborld.encoder;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map.Entry;

import com.apicatalog.cborld.encoder.EncoderException.Code;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.jsonld.uri.UriValidationPolicy;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

class EncoderContext {

    public static final Collection<String> get(final JsonObject document) throws EncoderException {
        return get(document, new LinkedHashSet<>());
    }

    static final Collection<String> get(final JsonObject document, Collection<String> contexts) throws EncoderException {

        for (final Entry<String, JsonValue> entry : document.entrySet()) {

            if (Keywords.CONTEXT.equals(entry.getKey())) {
                processContextValue(entry.getValue(), contexts);

            } else if (JsonUtils.isObject(entry.getValue())) {
                get(entry.getValue().asJsonObject(), contexts);
            }
        }
        return contexts;
    }

    static final void processContextValue(final JsonValue jsonValue, final Collection<String> result) throws EncoderException {

        if (JsonUtils.isString(jsonValue)) {
            final String uri = ((JsonString) jsonValue).getString();

            if (UriUtils.isAbsoluteUri(uri, UriValidationPolicy.Full)) {
                result.add(uri);
                return;
            }

        } else if (JsonUtils.isNonEmptyArray(jsonValue)) {

            for (final JsonValue item : jsonValue.asJsonArray()) {
                processContextValue(item, result);
            }
            return;

        } else if (JsonUtils.isObject(jsonValue)) {

            if (jsonValue.asJsonObject().size() == 1 && JsonUtils.isString(jsonValue.asJsonObject().get(Keywords.ID))) {

                final String id = ((JsonString) jsonValue.asJsonObject().get(Keywords.ID)).getString();

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
