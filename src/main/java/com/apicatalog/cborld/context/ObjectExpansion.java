/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.cborld.context;

import java.net.URI;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.Utils;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

final class ObjectExpansion {

    // mandatory
    private ActiveContext activeContext;
    private JsonValue propertyContext;
    private JsonObject element;
    private String activeProperty;
    private URI baseUrl;

    // optional
    private boolean ordered;
    private boolean fromMap;

    private ObjectExpansion(final ActiveContext activeContext, final JsonValue propertyContext, final JsonObject element,
            final String activeProperty, final URI baseUrl) {
        this.activeContext = activeContext;
        this.propertyContext = propertyContext;
        this.element = element;
        this.activeProperty = activeProperty;
        this.baseUrl = baseUrl;

        // default values
        this.ordered = false;
        this.fromMap = false;
    }

    public static final ObjectExpansion with(final ActiveContext activeContext, final JsonValue propertyContext,
            final JsonObject element, final String activeProperty, final URI baseUrl) {
        return new ObjectExpansion(activeContext, propertyContext, element, activeProperty, baseUrl);
    }

    public ObjectExpansion ordered(boolean value) {
        this.ordered = value;
        return this;
    }

    public ObjectExpansion fromMap(boolean value) {
        this.fromMap = value;
        return this;
    }

    public JsonValue expand() throws JsonLdError {

        initPreviousContext();

        initPropertyContext();

        initLocalContext();

        // 10.
        final ActiveContext typeContext = activeContext;

        final String typeKey = processTypeScoped(typeContext);

        final JsonMapBuilder result = JsonMapBuilder.create();

        ObjectExpansion1314
                    .with(activeContext, element, activeProperty, baseUrl)
                    .result(result)
                    .ordered(ordered)
                    .expand();

        return result.build();
    }

    private void initPropertyContext() throws JsonLdError {
        // 8.
        if (propertyContext != null) {

            activeContext = activeContext
                                .newContext()
                                .overrideProtected(true)
                                .create(
                                    propertyContext,
                                    activeContext
                                            .getTerm(activeProperty)
                                            .map(TermDefinition::getBaseUrl)
                                            .orElse(null)
                                        );
        }
    }

    private void initPreviousContext() throws JsonLdError {

        // 7. If active context has a previous context, the active context is not
        // propagated.
        // If from map is undefined or false, and element does not contain an entry
        // expanding to @value,
        // and element does not consist of a single entry expanding to @id (where
        // entries are IRI expanded),
        // set active context to previous context from active context,
        // as the scope of a term-scoped context does not apply when processing new node
        // objects.
        if (activeContext.getPreviousContext() != null && !fromMap) {

            boolean revert = true;

            for (final String key : Utils.index(element.keySet(), true)) {

                final String expandedKey =
                            activeContext
                                .uriExpansion()
                                .vocab(true)
                                .expand(key);

                if (Keywords.VALUE.equals(expandedKey) || (Keywords.ID.equals(expandedKey) && (element.size() == 1))) {
                    revert = false;
                    break;
                }
            }

            if (revert) {
                activeContext = activeContext.getPreviousContext();
            }
        }
    }

    private void initLocalContext() throws JsonLdError {
        // 9.
        if (element.containsKey(Keywords.CONTEXT)) {

            activeContext = activeContext
                                .newContext()
                                .create(element.get(Keywords.CONTEXT), baseUrl);
        }
    }

    private String processTypeScoped(final ActiveContext typeContext) throws JsonLdError {

        String typeKey = null;

        // 11.
        for (final String key : Utils.index(element.keySet(), true)) {

            final String expandedKey =
                        activeContext
                            .uriExpansion()
                            .vocab(true)
                            .expand(key);

            if (!Keywords.TYPE.equals(expandedKey)) {
                continue;

            } else if (typeKey == null) {
                typeKey = key;
            }

            // 11.2
            final List<String> terms = JsonUtils
                                            .toStream(element.get(key))
                                            .filter(JsonUtils::isString)
                                            .map(JsonString.class::cast)
                                            .map(JsonString::getString)
                                            .sorted()
                                            .collect(Collectors.toList());

            for (final String term : terms) {

                Optional<JsonValue> localContext = typeContext.getTerm(term).map(TermDefinition::getLocalContext);

                if (localContext.isPresent()) {

                    Optional<TermDefinition> valueDefinition = activeContext.getTerm(term);

                    activeContext =
                            activeContext
                                .newContext()
                                .propagate(false)
                                .create(localContext.get(),
                                        valueDefinition
                                                .map(TermDefinition::getBaseUrl)
                                                .orElse(null)
                                        );
                }
            }
        }

        return typeKey;
    }

    private String findInputType(final String typeKey) throws JsonLdError {

        // Initialize input type to expansion of the last value of the first entry in
        // element
        // expanding to @type (if any), ordering entries lexicographically by key. Both
        // the key and
        // value of the matched entry are IRI expanded.
        if (typeKey != null) {

            final JsonValue type = element.get(typeKey);

            if (JsonUtils.isArray(type)) {

                final String lastValue = type.asJsonArray()
                                .stream()
                                .filter(JsonUtils::isString)
                                .map(JsonString.class::cast)
                                .map(JsonString::getString)
                                .sorted()
                                .reduce((first, second) -> second)
                                .orElse(null);

                if (lastValue != null) {
                    return activeContext.uriExpansion().vocab(true).expand(lastValue);
                }

            } else if (JsonUtils.isString(type)) {
                return activeContext.uriExpansion().vocab(true).expand(((JsonString) type).getString());
            }
        }

        return null;
    }
}