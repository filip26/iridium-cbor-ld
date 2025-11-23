package com.apicatalog.cborld;

import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.java.NativeAdapter;

@DisplayName("CBOR-LD Test Suite")
@TestMethodOrder(OrderAnnotation.class)
class TestSuite {

    static final String BASE = "https://github.com/filip26/iridum-cbor-ld/";
    static final String VOCAB = "https://github.com/filip26/iridium-cbor-ld/tests/vocab#";

    @DisplayName("Encoder")
    @ParameterizedTest(name = "{0}")
    @MethodSource({ "encoderManifest" })
    @Order(1)
    void encode(TestCase testCase) {
        new JunitRunner(testCase).execute();
    }

    @DisplayName("Decoder")
    @ParameterizedTest(name = "{0}")
    @MethodSource({ "decoderManifest" })
    @Order(2)
    void decode(TestCase testCase) {
        new JunitRunner(testCase).execute();
    }

    static final Stream<TestCase> encoderManifest() throws JsonLdException, IOException, TreeIOException {
        return manifest("encoder-manifest.jsonld");
    }

    static final Stream<TestCase> decoderManifest() throws JsonLdException, IOException, TreeIOException {
        return manifest("decoder-manifest.jsonld");
    }

    static final Stream<TestCase> manifest(String name) throws JsonLdException, IOException, TreeIOException {
        try (final var is = TestSuite.class.getResourceAsStream(name)) {
            final var manifest = JsonLd.expand(JunitRunner.JSON_PARSER.parse(is), Options.newOptions()
                    .base(BASE)
                    .loader(JunitRunner.TEST_LOADER));

            return ((Collection<?>) ((Map<?, ?>) ((Collection<?>) manifest).iterator().next()).get(
                    "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#entries")).stream()
                    .map(test -> TestCase.of(test, NativeAdapter.instance()));
        }
    }
}
