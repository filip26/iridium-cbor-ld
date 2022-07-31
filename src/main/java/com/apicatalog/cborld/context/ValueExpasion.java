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

import java.util.Collection;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.Json;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

final class ValueExpasion {

    // required
    private final ActiveContext activeContext;

    // runtime
    private Optional<TermDefinition> definition;
    
    private final Collection<Collection<String>> appliedContexts;

    private ValueExpasion(final ActiveContext activeContext, Collection<Collection<String>> appliedContexts) {
        this.activeContext = activeContext;
        this.appliedContexts = appliedContexts;
    }

    public static final ValueExpasion with(final ActiveContext activeContext, Collection<Collection<String>> appliedContexts) {
        return new ValueExpasion(activeContext, appliedContexts);
    }

    public JsonValue expand(final JsonValue value, final String activeProperty) throws JsonLdError {
        
        definition = activeContext.getTerm(activeProperty);

        final Optional<String> typeMapping = definition.map(TermDefinition::getTypeMapping);

        if (typeMapping.isPresent()) {

            // 1.
            if (Keywords.ID.equals(typeMapping.get())) {

                String idValue = null;

                if (JsonUtils.isString(value)) {
                    idValue = ((JsonString) value).getString();

                // custom extension allowing to process numeric ids
                } else if (activeContext.getOptions().isNumericId() && JsonUtils.isNumber(value)) {
                    idValue = ((JsonNumber) value).toString();
                }

                if (idValue != null) {
                    final String expandedValue = UriExpansion
                                                    .with(activeContext, appliedContexts)
                                                    .documentRelative(true)
                                                    .vocab(false)
                                                    .expand(idValue);

                    return Json.createObjectBuilder().add(Keywords.ID, expandedValue)
                            .add(Keywords.TYPE,  Keywords.ID).build();
                }

            // 2.
            } else if (Keywords.VOCAB.equals(typeMapping.get()) && JsonUtils.isString(value)) {

                String expandedValue = UriExpansion
                                            .with(activeContext, appliedContexts)
                                            .documentRelative(true)
                                            .vocab(true)
                                            .expand(((JsonString) value).getString());

                return Json.createObjectBuilder().add(Keywords.ID, expandedValue)
                        .add(Keywords.TYPE,  Keywords.VOCAB).build();
            }
        }

        // 3.
        final JsonObjectBuilder result = Json.createObjectBuilder().add(Keywords.VALUE, value);

        // 4.
        if (typeMapping
                    .filter(t -> !Keywords.ID.equals(t) && !Keywords.VOCAB.equals(t) && !Keywords.NONE.equals(t))
                    .isPresent()) {

            result.add(Keywords.TYPE, typeMapping.get());
            return result.build();
            
            // 5.
        } else if (JsonUtils.isString(value)) {
            buildStringValue(result);
            return result.build();
        }

        return null;
    }
    
    private void buildStringValue(final JsonObjectBuilder result) {

        // 5.1.
        final JsonValue language = definition
                                            .map(TermDefinition::getLanguageMapping)
                                            .orElseGet(() -> activeContext.getDefaultLanguage() != null
                                                                ? Json.createValue(activeContext.getDefaultLanguage())
                                                                : null);
        // 5.2.
        final DirectionType direction = definition
                                            .map(TermDefinition::getDirectionMapping)
                                            .orElseGet(() -> activeContext.getDefaultBaseDirection());

        // 5.3.
        if (JsonUtils.isNotNull(language)) {
            result.add(Keywords.LANGUAGE, language);
        }

        // 5.4.
        if (direction != null && !DirectionType.NULL.equals(direction)) {
            result.add(Keywords.DIRECTION, Json.createValue(direction.name().toLowerCase()));
        }
    }
}
