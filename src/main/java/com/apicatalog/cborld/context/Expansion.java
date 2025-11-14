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

final class Expansion {

//    // mandatory
//    private ActiveContext activeContext;
//    private String activeProperty;
//    private URI baseUrl;
//
//    private Object element;
//    private final TreeAdapter adapter;
//
//    // optional
//    private boolean ordered;
//    private boolean fromMap;
//
//    private Consumer<Collection<String>> appliedContexts;
//    private TypeKeyNameMapper typeMapper;
//
//    private Expansion(final ActiveContext activeContext, final Object element, final TreeAdapter adapter, final String activeProperty,
//            final URI baseUrl, Consumer<Collection<String>> appliedContexts, TypeKeyNameMapper typeMapper) {
//        this.activeContext = activeContext;
//        this.element = element;
//        this.adapter = adapter;
//        this.activeProperty = activeProperty;
//        this.baseUrl = baseUrl;
//
//        this.appliedContexts = appliedContexts;
//        this.typeMapper = typeMapper;
//
//        // default values
//        this.ordered = false;
//        this.fromMap = false;
//    }
//
//    public static final Expansion with(
//            final ActiveContext activeContext,
//            final Object element,
//            final TreeAdapter adapter,
//            final String activeProperty,
//            final URI baseUrl, Consumer<Collection<String>> appliedContexts,
//            final TypeKeyNameMapper typeMapper) {
//
//        return new Expansion(activeContext, element, adapter, activeProperty, baseUrl, appliedContexts, typeMapper);
//    }
//
//    public Expansion ordered(boolean value) {
//        this.ordered = value;
//        return this;
//    }
//
//    public Expansion fromMap(boolean value) {
//        this.fromMap = value;
//        return this;
//    }
//
//    protected JsonValue compute() throws JsonLdError {
//
//        // 1. If element is null, return null
//        if (element == null) {
//            return JsonValue.NULL;
//        }
//
//        final NodeType dataType = adapter.type(element);
//
//        // 5. If element is an array,
//        if (NodeType.COLLECTION == dataType) {
//
//            return ArrayExpansion
//                    .with(activeContext, element, adapter, activeProperty, baseUrl, appliedContexts, typeMapper)
//                    .ordered(ordered)
//                    .fromMap(fromMap)
//                    .expand();
//        }
//
//        // 3. If active property has a term definition in active context with a local
//        // context, initialize property-scoped context to that local context.
//        final JsonValue propertyContext = activeContext
//                .getTerm(activeProperty)
//                .map(TermDefinition::getLocalContext)
//                .orElse(null);
//
//        if (JsonUtils.isNotNull(propertyContext)) {
//            if (JsonUtils.isObject(propertyContext)) {
//                appliedContexts.accept(propertyContext.asJsonObject().keySet());
//            }
//        }
//
//        // 4. If element is a scalar
//        if (dataType.isScalar()) {
//            return ScalarExpansion
//                    .with(activeContext, propertyContext, element, adapter, activeProperty, appliedContexts)
//                    .expand();
//        }
//
//        // 6. Otherwise element is a map
//        return ObjectExpansion
//                .with(activeContext, propertyContext, element, adapter, 
//                        activeProperty, baseUrl,
//                        appliedContexts,
//                        typeMapper)
//                .ordered(ordered)
//                .fromMap(fromMap)
//                .expand();
//    }
//
//    public TypeMap typeMapping() throws JsonLdError {
//        return new DynamicTypeMap(compute());
//    }
}