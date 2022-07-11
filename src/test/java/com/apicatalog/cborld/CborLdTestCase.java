package com.apicatalog.cborld;

import java.net.URI;
import java.util.Set;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class CborLdTestCase {

    public URI id;

    public String name;

    public URI input;

    public Set<String> type;

    public Object result;

    public static CborLdTestCase of(JsonObject test, JsonObject manifest, DocumentLoader loader) {

        final CborLdTestCase testCase = new CborLdTestCase();

        testCase.id = URI.create(test.getString(Keywords.ID));

        testCase.type = test.getJsonArray(Keywords.TYPE).stream().map(JsonString.class::cast).map(JsonString::getString)
                .collect(Collectors.toSet());

        testCase.name = test.getJsonArray("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name")
                .getJsonObject(0).getString(Keywords.VALUE);

        testCase.input = URI.create(test.getJsonArray("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action")
                .getJsonObject(0).getString(Keywords.ID));

        if (test.containsKey("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#result")) {
            final JsonObject result = test
                    .getJsonArray("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#result").getJsonObject(0);

            JsonValue resultValue = result.getOrDefault(Keywords.ID, result.getOrDefault(Keywords.VALUE, null));

            if (JsonUtils.isString(resultValue)) {
                testCase.result = ((JsonString) resultValue).getString();

            } else {
                testCase.result = !JsonValue.ValueType.FALSE.equals(resultValue.getValueType());
            }
        }

        if (test.containsKey("https://github.com/filip26/iron-verifiable-credentials/tests/vocab#options")) {

            final JsonObject options = test
                    .getJsonArray("https://github.com/filip26/iron-verifiable-credentials/tests/vocab#options")
                    .getJsonObject(0);

        }

        return testCase;
    }

    @Override
    public String toString() {
        return id.getFragment() + ": " + name;
    }
}
