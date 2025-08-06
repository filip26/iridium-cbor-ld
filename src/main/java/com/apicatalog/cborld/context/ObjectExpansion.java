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
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import com.apicatalog.cborld.mapping.TypeKeyNameMapper;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.Utils;
import com.apicatalog.lq.Data;
import com.apicatalog.lq.Q;
import com.apicatalog.lq.jakarta.JakartaAdapter;

import jakarta.json.JsonString;
import jakarta.json.JsonValue;

final class ObjectExpansion {

    // mandatory
    private ActiveContext activeContext;
    private JsonValue propertyContext;
    private Data element;
    private String activeProperty;
    private URI baseUrl;

    private Consumer<Collection<String>> appliedContexts;
    private TypeKeyNameMapper typeMapper;

    // optional
    private boolean ordered;
    private boolean fromMap;

    private ObjectExpansion(final ActiveContext activeContext, final JsonValue propertyContext, final Data element,
            final String activeProperty, final URI baseUrl, Consumer<Collection<String>> appliedContexts, TypeKeyNameMapper typeMapper) {
        this.activeContext = activeContext;
        this.propertyContext = propertyContext;
        this.element = element;
        this.activeProperty = activeProperty;
        this.baseUrl = baseUrl;

        this.appliedContexts = appliedContexts;
        this.typeMapper = typeMapper;

        // default values
        this.ordered = false;
        this.fromMap = false;
    }

    public static final ObjectExpansion with(final ActiveContext activeContext, final JsonValue propertyContext,
            final Data element, final String activeProperty, final URI baseUrl, Consumer<Collection<String>> appliedContexts, TypeKeyNameMapper typeMapper) {
        return new ObjectExpansion(activeContext, propertyContext, element, activeProperty, baseUrl, appliedContexts, typeMapper);
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

        if (activeProperty != null && typeMapper != null) {
            typeMapper.beginMap(activeProperty);
        }

        initPreviousContext();

        initPropertyContext();

        initLocalContext();

        // 10.
        final ActiveContext typeContext = activeContext;

        processTypeScoped(typeContext);

        final JsonMapBuilder result = JsonMapBuilder.create();

        ObjectExpansion1314
                .with(activeContext, element, activeProperty, baseUrl, appliedContexts, typeMapper)
                .result(result)
                .ordered(ordered)
                .expand();

        if (activeProperty != null && typeMapper != null) {
            typeMapper.end();
        }

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
                                    .orElse(null));
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

            for (final String key : Utils.index(Q.keys(element), true)) {

                final String expandedKey = UriExpansion
                        .with(activeContext, appliedContexts)
                        .vocab(true)
                        .expand(key);

                if (Keywords.VALUE.equals(expandedKey) || (Keywords.ID.equals(expandedKey) && (Q.size(element) == 1))) {
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
        if (Q.contains(element, Keywords.CONTEXT)) {

            final JsonValue jsonContext = JakartaAdapter.toJson(Q.value(element, Keywords.CONTEXT));

            for (final JsonValue context : JsonUtils.toJsonArray(jsonContext)) {
                final ActiveContext ac = new ActiveContext(activeContext.getBaseUri(), activeContext.getBaseUrl(), activeContext.runtime())
                        .newContext()
                        .create(context, baseUrl);
                appliedContexts.accept(ac.getTerms());
            }

            activeContext = activeContext
                    .newContext()
                    .create(jsonContext, baseUrl);
        }
    }

    private String processTypeScoped(final ActiveContext typeContext) throws JsonLdError {

        if (Q.isEmpty(element)) {
            return null;
        }

        String typeKey = null;

        final Collection<String> keys = Q.keys(element);

        for (final String key : Utils.index(keys, true)) {

            final String expandedKey = UriExpansion
                    .with(activeContext, appliedContexts)
                    .vocab(true)
                    .expand(key);

            if (!Keywords.TYPE.equals(expandedKey)) {
                continue;

            } else if (typeKey == null) {
                typeKey = key;
            }

            if (typeMapper != null) {
                typeMapper.typeKeyName(key);
            }

            final Data entry = Q.value(element, key);

            final JsonValue entryValue = JakartaAdapter.toJson(entry);

            // 11.2
            final List<String> terms = JsonUtils.toStream(entryValue)
                    .filter(JsonUtils::isString)
                    .map(JsonString.class::cast)
                    .map(JsonString::getString)
                    .sorted()
                    .collect(Collectors.toList());

            for (final String term : terms) {

                Optional<JsonValue> localContext = typeContext.getTerm(term).map(TermDefinition::getLocalContext);

                if (localContext.isPresent()) {

                    final JsonValue lc = localContext.get();

                    if (JsonUtils.isObject(lc)) {
                        appliedContexts.accept(lc.asJsonObject().keySet());
                    }

                    Optional<TermDefinition> valueDefinition = activeContext.getTerm(term);

                    activeContext = activeContext
                            .newContext()
                            .propagate(false)
                            .create(lc,
                                    valueDefinition
                                            .map(TermDefinition::getBaseUrl)
                                            .orElse(null));
                }
            }
        }

        return typeKey;
    }
}