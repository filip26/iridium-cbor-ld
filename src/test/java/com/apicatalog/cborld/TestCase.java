package com.apicatalog.cborld;

import java.net.URI;
import java.util.Set;
import java.util.stream.Collectors;

import com.apicatalog.cborld.config.ConfigV1;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.TreeAdapter;

class TestCase {

    public URI id;

    public String name;

    public URI input;

    public Set<String> type;

    public URI result;

    public boolean compactArrays = ConfigV1.INSTANCE.isCompactArrays();

    public String config;

    public static TestCase of(Object test, TreeAdapter adapter) {

        final TestCase testCase = new TestCase();

        testCase.id = URI.create(adapter.stringValue(adapter.property(Keywords.ID, test)));

        testCase.type = adapter.asStream(adapter.property(Keywords.TYPE, test))
                .map(adapter::asString)
                .collect(Collectors.toUnmodifiableSet());

        testCase.name = adapter.stringValue(
                adapter.property(Keywords.VALUE,
                        adapter.singleElement(
                                adapter.property("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name",
                                        test))));

        testCase.input = URI.create(adapter.stringValue(
                adapter.property(Keywords.ID,
                        adapter.singleElement(
                                adapter.property("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action",
                                        test)))));

        var result = adapter.property("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#result", test);
        if (result != null) {
            result = adapter.singleElement(result);

            var value = adapter.property(Keywords.ID, result);
            if (value == null) {
                value = adapter.property(Keywords.VALUE, result);
            }

            if (adapter.isString(value)) {
                testCase.result = URI.create(adapter.stringValue(value));
            }
        }

        var options = adapter.property("https://github.com/filip26/iridium-cbor-ld/tests/vocab#option", test);
        if (options != null) {
            options = adapter.singleElement(options);

            var compactArrays = adapter.property("https://github.com/filip26/iridium-cbor-ld/tests/vocab#compactArrays", options);
            if (compactArrays != null) {
                compactArrays = adapter.singleElement(compactArrays);
                compactArrays = adapter.property(Keywords.VALUE, compactArrays);
                if (adapter.isNull(compactArrays)) {
                    testCase.compactArrays = ConfigV1.INSTANCE.isCompactArrays();
                } else {
                    testCase.compactArrays = adapter.isTrue(compactArrays);
                }
            }

            var config = adapter.property("https://github.com/filip26/iridium-cbor-ld/tests/vocab#config", options);
            if (config != null) {
                config = adapter.singleElement(config);

                testCase.config = adapter.stringValue(adapter.property(Keywords.VALUE, config));
            }
        }
        return testCase;
    }

    @Override
    public String toString() {
        return id.getFragment() + ": " + name;
    }
}
