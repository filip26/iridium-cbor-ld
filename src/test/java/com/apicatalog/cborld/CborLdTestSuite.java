package com.apicatalog.cborld;

import java.io.IOException;
import java.io.InputStream;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.JsonDocument;

import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

@DisplayName("CBOR-LD Test Suite")
@TestMethodOrder(OrderAnnotation.class)
class CborLdTestSuite {

    static final String BASE = "https://github.com/filip26/iridum-cbor-ld/";
    static final String VOCAB = "https://github.com/filip26/iridium-cbor-ld/tests/vocab#";

    @DisplayName("Encoder")
    @ParameterizedTest(name = "{0}")
    @MethodSource({ "encoderManifest" })
    @Order(1)
    void encode(CborLdTestCase testCase) {
        new CborLdTestRunnerJunit(testCase).execute();
    }

    @DisplayName("Decoder")
    @ParameterizedTest(name = "{0}")
    @MethodSource({ "decoderManifest" })
    @Order(2)
    void decode(CborLdTestCase testCase) {
        new CborLdTestRunnerJunit(testCase).execute();
    }

    static final Stream<CborLdTestCase> encoderManifest() throws JsonLdError, IOException {
        return manifest("encoder-manifest.jsonld");
    }

    static final Stream<CborLdTestCase> decoderManifest() throws JsonLdError, IOException {
        return manifest("decoder-manifest.jsonld");
    }

    static final Stream<CborLdTestCase> manifest(String name) throws JsonLdError, IOException {

        try (final InputStream is = CborLdTestSuite.class.getResourceAsStream(name)) {

            final JsonObject manifest = JsonLd.expand(JsonDocument.of(is))
                        .base(BASE)
                        .loader(CborLdTestRunnerJunit.LOADER)
                        .get()
                        .getJsonObject(0);

            return manifest
                .asJsonObject().getJsonArray("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#entries")
                .stream()
                .map(JsonValue::asJsonObject)
                .map(test -> CborLdTestCase.of(test, manifest, CborLdTestRunnerJunit.LOADER));
        }
    }
}
