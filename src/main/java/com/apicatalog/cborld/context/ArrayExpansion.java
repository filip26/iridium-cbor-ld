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
import java.util.function.Consumer;

import com.apicatalog.cborld.mapping.TypeKeyNameMapper;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.tree.io.NodeAdapter;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonValue;

final class ArrayExpansion {

    // mandatory
    private ActiveContext activeContext;
    private String activeProperty;
    private URI baseUrl;

    private final Object element;
    private final NodeAdapter adapter;

    private final Consumer<Collection<String>> appliedContexts;
    private final TypeKeyNameMapper typeMapper;

    // optional
    private boolean ordered;
    private boolean fromMap;

    private ArrayExpansion(final ActiveContext activeContext, final Object element, NodeAdapter adapter, final String activeProperty,
            final URI baseUrl, Consumer<Collection<String>> appliedContexts, TypeKeyNameMapper typeMapper) {
        this.activeContext = activeContext;
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

    public static final ArrayExpansion with(
            final ActiveContext activeContext, 
            final Object element,
            final NodeAdapter adapter,
            final String activeProperty, 
            final URI baseUrl, 
            final Consumer<Collection<String>> appliedContexts, 
            final TypeKeyNameMapper typeMapper) {
        return new ArrayExpansion(activeContext, element, adapter, activeProperty, baseUrl, appliedContexts, typeMapper);
    }

    public ArrayExpansion ordered(boolean value) {
        this.ordered = value;
        return this;
    }

    public ArrayExpansion fromMap(boolean value) {
        this.fromMap = value;
        return this;
    }

    public JsonArray expand() throws JsonLdError {

        if (adapter.isEmpty(element)) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }

        final JsonArrayBuilder result = Json.createArrayBuilder();

        // 5.2.
        for (final Object item : adapter.iterable(element)) {

            // 5.2.1
            JsonValue expanded = Expansion
                    .with(activeContext, item, adapter, activeProperty, baseUrl, appliedContexts, typeMapper)
                    .ordered(ordered)
                    .fromMap(fromMap)
                    .compute();

            // 5.2.3
            if (JsonUtils.isArray(expanded)) {

                // append array
                expanded
                        .asJsonArray()
                        .stream()
                        .filter(JsonUtils::isNotNull)
                        .forEach(result::add);

                // append non-null element
            } else if (JsonUtils.isNotNull(expanded)) {
                result.add(expanded);
            }
        }

        // 5.3
        return result.build();
    }
}
