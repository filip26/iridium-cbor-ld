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
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.function.Consumer;

import com.apicatalog.cborld.mapping.TypeKeyNameMapper;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.JakartaMaterializer;
import com.apicatalog.tree.io.NodeAdapter;
import com.apicatalog.tree.io.NodeModel;

import jakarta.json.JsonValue;

final class ObjectExpansion2 {

    final JakartaMaterializer jakarta = new JakartaMaterializer();
    
    // mandatory
    private ActiveContext activeContext;
    private JsonValue propertyContext;
    private String activeProperty;
    private URI baseUrl;

    private Object element;
    private final NodeAdapter adapter;

    private Consumer<Collection<String>> appliedContexts;
    private TypeKeyNameMapper typeMapper;

    // optional
    private boolean ordered;
    private boolean fromMap;

    private ObjectExpansion2(final ActiveContext activeContext, final JsonValue propertyContext,
            final Object element, final NodeAdapter adapter,
            final String activeProperty, final URI baseUrl, 
            Consumer<Collection<String>> appliedContexts, 
            TypeKeyNameMapper typeMapper) {
        this.activeContext = activeContext;
        this.propertyContext = propertyContext;
        this.element = element;
        this.adapter = adapter;
        this.activeProperty = activeProperty;
        this.baseUrl = baseUrl;

        this.appliedContexts = appliedContexts;
        this.typeMapper = typeMapper;

        // default values
        this.ordered = false;
        this.fromMap = false;
    }

    public static final ObjectExpansion2 with(final ActiveContext activeContext, final JsonValue propertyContext,
            final Object element, final NodeAdapter adapter, final String activeProperty, final URI baseUrl, Consumer<Collection<String>> appliedContexts, TypeKeyNameMapper typeMapper) {
        return new ObjectExpansion2(activeContext, propertyContext, element, adapter, activeProperty, baseUrl, appliedContexts, typeMapper);
    }

    public ObjectExpansion2 ordered(boolean value) {
        this.ordered = value;
        return this;
    }

    public ObjectExpansion2 fromMap(boolean value) {
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
                .with(activeContext, element, adapter, activeProperty, baseUrl, appliedContexts, typeMapper)
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

            final Iterator<Entry<?, ?>> entries = adapter.propertyStream(element)
                    .sorted(NodeModel.comparingEntry(e -> adapter.asString(e.getKey())))
                    .iterator();
            
            while (entries.hasNext()) {

                final Entry<?, ?> entry = entries.next();
                
                final String expandedKey = UriExpansion
                        .with(activeContext, appliedContexts)
                        .vocab(true)
                        .expand(adapter.asString(entry.getKey()));

                if (Keywords.VALUE.equals(expandedKey) || (Keywords.ID.equals(expandedKey) && (adapter.size(element) == 1))) {
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
        final Object contextElement = adapter.property(Keywords.CONTEXT, element);
System.out.println(">> " + contextElement + ", " + element);
        if (contextElement != null) {

            jakarta.node(contextElement, adapter);
            
            final JsonValue jsonContext = jakarta.json();
            System.out.println("> " + jsonContext);
            for (final JsonValue context : JsonUtils.toJsonArray(jsonContext)) {
                final ActiveContext ac = new ActiveContext(activeContext.getBaseUri(), activeContext.getBaseUrl(), activeContext.runtime())
                        .newContext()
                        .create(context, baseUrl);
                System.out.println(": " + context + ", " + ac.getTerms());
                appliedContexts.accept(ac.getTerms());
            }

            activeContext = activeContext
                    .newContext()
                    .create(jsonContext, baseUrl);
        }
    }

    private String processTypeScoped(final ActiveContext typeContext) throws JsonLdError {

        if (adapter.isEmpty(element)) {
            return null;
        }

        String typeKey = null;

        final Iterator<Entry<?, ?>> entries = adapter.propertyStream(element)
                .sorted(NodeModel.comparingEntry(e -> adapter.asString(e.getKey())))
                .iterator();
        
        while (entries.hasNext()) {

            final Entry<?, ?> entry = entries.next();

            final String key = adapter.asString(entry.getKey());
            
            final String expandedKey = UriExpansion
                    .with(activeContext, appliedContexts)
                    .vocab(true)
                    .expand(key);
System.out.println("::>>  " + key + ", " + expandedKey);
            if (!Keywords.TYPE.equals(expandedKey)) {
                continue;

            } else if (typeKey == null) {
                typeKey = key;
            }

            if (typeMapper != null) {
                typeMapper.typeKeyName(key);
            }

            // 11.2
            final Iterator<String> terms = adapter.asStream(entry.getValue())
                    .filter(adapter::isString)
                    .map(adapter::stringValue)
                    .sorted()
                    .iterator();

            while (terms.hasNext()) {
                
                final String term = terms.next();

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