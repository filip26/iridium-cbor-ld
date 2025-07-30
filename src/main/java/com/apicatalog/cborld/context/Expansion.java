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
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.lq.Q;
import com.apicatalog.lq.ValueHolder;

import jakarta.json.JsonValue;

final class Expansion {

    // mandatory
    private ActiveContext activeContext;
    private ValueHolder element;
    private String activeProperty;
    private URI baseUrl;

    // optional
    private boolean ordered;
    private boolean fromMap;

    private Consumer<Collection<String>> appliedContexts;
    private TypeKeyNameMapper typeMapper;

    private Expansion(final ActiveContext activeContext, final ValueHolder element, final String activeProperty,
            final URI baseUrl, Consumer<Collection<String>> appliedContexts, TypeKeyNameMapper typeMapper) {
        this.activeContext = activeContext;
        this.element = element;
        this.activeProperty = activeProperty;
        this.baseUrl = baseUrl;

        this.appliedContexts = appliedContexts;
        this.typeMapper = typeMapper;

        // default values
        this.ordered = false;
        this.fromMap = false;
    }

    public static final Expansion with(final ActiveContext activeContext, final ValueHolder element, final String activeProperty, final URI baseUrl, Consumer<Collection<String>> appliedContexts,
            TypeKeyNameMapper typeMapper) {
        return new Expansion(activeContext, element, activeProperty, baseUrl, appliedContexts, typeMapper);
    }

    public Expansion ordered(boolean value) {
        this.ordered = value;
        return this;
    }

    public Expansion fromMap(boolean value) {
        this.fromMap = value;
        return this;
    }

    protected JsonValue compute() throws JsonLdError {

        // 1. If element is null, return null
        if (Q.isNull(element)) {
            return JsonValue.NULL;
        }

        // 5. If element is an array,
        if (Q.isArray(element)) {

            return ArrayExpansion
                    .with(activeContext, element, activeProperty, baseUrl, appliedContexts, typeMapper)
                    .ordered(ordered)
                    .fromMap(fromMap)
                    .expand();
        }

        // 3. If active property has a term definition in active context with a local
        // context, initialize property-scoped context to that local context.
        final JsonValue propertyContext = activeContext
                .getTerm(activeProperty)
                .map(TermDefinition::getLocalContext)
                .orElse(null);

        if (JsonUtils.isNotNull(propertyContext)) {
            if (JsonUtils.isObject(propertyContext)) {
                appliedContexts.accept(propertyContext.asJsonObject().keySet());
            }
        }

        // 4. If element is a scalar
        if (Q.isScalar(element)) {

            return ScalarExpansion
                    .with(activeContext, propertyContext, element, activeProperty, appliedContexts)
                    .expand();
        }

        // 6. Otherwise element is a map
        return ObjectExpansion
                .with(activeContext, propertyContext, element, activeProperty, baseUrl, appliedContexts, typeMapper)
                .ordered(ordered)
                .fromMap(fromMap)
                .expand();
    }

    public TypeMap typeMapping() throws JsonLdError {
        return new DynamicTypeMap(compute());
    }
}