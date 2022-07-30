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
import java.util.Collections;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.ListObject;
import com.apicatalog.jsonld.lang.Utils;
import com.apicatalog.jsonld.lang.ValueObject;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

final class ObjectExpansion1314 {

    // mandatory
    private ActiveContext activeContext;

    private final JsonObject element;
    private final String activeProperty;
    private final URI baseUrl;

    private JsonMapBuilder result;

    // optional
    private boolean ordered;

    private ObjectExpansion1314(final ActiveContext activeContext, final JsonObject element,
            final String activeProperty, final URI baseUrl) {
        this.activeContext = activeContext;
        this.element = element;
        this.activeProperty = activeProperty;
        this.baseUrl = baseUrl;

        // default values
        this.ordered = false;
    }

    public static final ObjectExpansion1314 with(final ActiveContext activeContext, final JsonObject element,
            final String activeProperty, final URI baseUrl) {
        return new ObjectExpansion1314(activeContext, element, activeProperty, baseUrl);
    }

    public ObjectExpansion1314 ordered(boolean value) {
        this.ordered = value;
        return this;
    }

    public ObjectExpansion1314 result(JsonMapBuilder result) {
        this.result = result;
        return this;
    }

    public void expand() throws JsonLdError {

        // 13.
        for (final String key : Utils.index(element.keySet(), ordered)) {

            // 13.1.
            if (Keywords.CONTEXT.equals(key)) {
                continue;
            }

            // 13.2.
            String expandedProperty =
                        activeContext
                            .uriExpansion()
                            .documentRelative(false)
                            .vocab(true)
                            .expand(key);

            // 13.3.
            if (expandedProperty == null || (!expandedProperty.contains(":") && !Keywords.contains(expandedProperty))) {
                continue;
            }

            JsonValue value = element.get(key);

            // 13.4. If expanded property is a keyword:
            if (Keywords.contains(expandedProperty)) {

                JsonValue expandedType = value;

                // 13.4.1
                if (Keywords.REVERSE.equals(activeProperty)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_MAP);
                }

                // 13.4.4
                if (Keywords.ID.equals(expandedProperty)) {
                    result.add(key, Json.createValue(Keywords.ID));
                    continue;
                }

                // 13.4.4
                if (Keywords.TYPE.equals(expandedProperty)) {
                    result.add(key, Json.createValue(Keywords.TYPE));
                    continue;
                }

                // 13.4.5
                else if (Keywords.GRAPH.equals(expandedProperty)) {

                }

                // 13.4.6
                else if (Keywords.INCLUDED.equals(expandedProperty)) {

                    // 13.4.6.1
                    if (activeContext.inMode(JsonLdVersion.V1_0)) {
                        continue;
                    }

                    expandedType = Expansion
                                        .with(activeContext, value, null, baseUrl)
                                        .ordered(ordered)
                                        .compute();
                }

                // 13.4.11
                if (Keywords.LIST.equals(expandedProperty)) {

                }

                // 13.4.12
                if (Keywords.SET.equals(expandedProperty)) {

                }

                // 13.4.13
                if (Keywords.REVERSE.equals(expandedProperty)) {

                    continue;
                }

                // 13.4.14
                if (Keywords.NEST.equals(expandedProperty)) {
                    continue;
                }

                // 13.4.16
                if (expandedType != null) {
                    result.add(key, expandedType);                        
                }

                // 13.4.17
                continue;
            }

            // 13.5.
            final Optional<TermDefinition> keyTermDefinition = activeContext.getTerm(key);

            final Collection<String> containerMapping = keyTermDefinition
                                                        .map(TermDefinition::getContainerMapping)
                                                        .orElseGet(() -> Collections.emptyList());

            JsonValue expandedType = null;

            // 13.6.
            if (keyTermDefinition.map(TermDefinition::getTypeMapping).filter(Keywords.JSON::equals).isPresent()) {

                expandedType = Json.createValue(Keywords.JSON);

            // 13.7.
            } else if (containerMapping.contains(Keywords.LANGUAGE) && JsonUtils.isObject(value)) {


            // 13.8.
            } else if ((containerMapping.contains(Keywords.INDEX) || containerMapping.contains(Keywords.TYPE)
                    || containerMapping.contains(Keywords.ID)) && JsonUtils.isObject(value)) {


            // 13.9.
            } else {

                expandedType = Expansion
                                    .with(activeContext, value, key, baseUrl)
                                    .ordered(ordered)
                                    .compute();
            }

            // 13.10.
            if (JsonUtils.isNull(expandedType)) {
                continue;
            }

            // 13.13.
            if (keyTermDefinition.filter(TermDefinition::isReverseProperty).isPresent()) {

                // 13.13.3.
                expandedType = JsonUtils.toJsonArray(expandedType);

                // 13.13.4.
                for (JsonValue item : expandedType.asJsonArray()) {

                    // 13.13.4.1.
                    if (ListObject.isListObject(item) || ValueObject.isValueObject(item)) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_VALUE);
                    }

                    // 13.13.4.3.
                    result.getMapBuilder(Keywords.REVERSE).add(key, item);
                }

            // 13.14
            } else if (expandedType != null) {
                result.add(key, expandedType);
            }
        }

    }
}
