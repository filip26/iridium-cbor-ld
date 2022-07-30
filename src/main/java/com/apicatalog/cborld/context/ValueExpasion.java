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

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.Json;
import jakarta.json.JsonValue;

final class ValueExpasion {

    // required
    private final ActiveContext activeContext;

    private ValueExpasion(final ActiveContext activeContext) {
        this.activeContext = activeContext;
    }

    public static final ValueExpasion with(final ActiveContext activeContext) {
        return new ValueExpasion(activeContext);
    }

    public JsonValue expand(final JsonValue value, final String activeProperty) throws JsonLdError {
        return activeContext.getTerm(activeProperty).map(TermDefinition::getTypeMapping)
            .filter(typeMapping -> !Keywords.NONE.equals(typeMapping))
            .map(typeMapping -> Json.createValue(typeMapping))
            .orElse(null);
    }
}
