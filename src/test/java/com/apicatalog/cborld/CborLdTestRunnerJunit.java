package com.apicatalog.cborld;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeFalse;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.apicatalog.cbor.CborComparison;
import com.apicatalog.cbor.CborWriter;
import com.apicatalog.cborld.config.DefaultConfig;
import com.apicatalog.cborld.config.LegacyConfigV05;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.encoder.EncoderError;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.json.JsonLdComparison;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.jsonld.loader.HttpLoader;
import com.apicatalog.jsonld.loader.SchemeRouter;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

class CborLdTestRunnerJunit {

    private final CborLdTestCase testCase;

    public static final DocumentLoader LOADER;

    public static final Encoder ENCODER_V1;

    public static final Encoder ENCODER_V06;

    public static final Encoder ENCODER_V05;
    
    public static final Encoder ENCODER_V05_NOCA;

    static {
        StaticContextLoader.set("https://w3id.org/utopia/v2", CborLdTestRunnerJunit.class, "utopia-v2.jsonld");
        StaticContextLoader.set("https://w3id.org/age/v1", CborLdTestRunnerJunit.class, "age-v1.jsonld");

        LOADER = new UriBaseRewriter(
                CborLdTest.BASE,
                "classpath:",
                new UriBaseRewriter("https://raw.githubusercontent.com/filip26/iridium-cbor-ld/main/src/test/resources/com/apicatalog/cborld/",
                        "classpath:",
                        new SchemeRouter()
                                .set("http", HttpLoader.defaultInstance())
                                .set("https", HttpLoader.defaultInstance())
                                .set("classpath", new ClasspathLoader())));

        ENCODER_V1 = CborLd.createEncoder()
                .loader(LOADER)
                .build();

        ENCODER_V06 = CborLd.createEncoder(CborLdVersion.V06)
                .loader(LOADER)
                .dictionary(Barcodes.DICTIONARY)
                .build();

        ENCODER_V05 = CborLd.createEncoder(CborLdVersion.V05)
                .loader(LOADER)
                .build();
        
        ENCODER_V05_NOCA = CborLd.createEncoder(CborLdVersion.V05)
                .loader(LOADER)
                .compactArray(false)
                .build();
        
    }

    public CborLdTestRunnerJunit(CborLdTestCase testCase) {
        this.testCase = testCase;
    }

    public void execute() {

        assertNotNull(testCase.type);
        assertNotNull(testCase.input);

        try {
            if (testCase.type.contains(CborLdTest.VOCAB + "EncoderTest")) {

                assumeFalse("t0025".equals(testCase.id.getFragment()));
                assumeFalse("t0026".equals(testCase.id.getFragment()));

                Document document = LOADER.loadDocument(testCase.input, new DocumentLoaderOptions());

                JsonObject object = document.getJsonContent().orElseThrow(IllegalStateException::new).asJsonObject();

                Encoder encoder = getEncoder(testCase.config, testCase.compactArrays);

                byte[] bytes = encoder.encode(object);

                if (testCase.type.stream().noneMatch(o -> o.endsWith("PositiveEvaluationTest"))) {
                    fail("Expected error code [" + testCase.result + "].");
                    return;
                }

                assertNotNull(bytes);

                Document expected = LOADER.loadDocument(testCase.result, new DocumentLoaderOptions());

                assertNotNull(expected);
                assertEquals(CborLdDocument.MEDIA_TYPE, expected.getContentType());

                final byte[] expectedBytes = ((CborLdDocument) expected).getByteArray();

                final boolean match = equalCborLdHeader(expectedBytes, bytes) && CborComparison.equals(expectedBytes, bytes);

                if (!match) {
                    write(testCase, bytes, ((CborLdDocument) expected).getByteArray());
                }

                assertTrue(match, "The expected result does not match.");

            } else if (testCase.type.contains(CborLdTest.VOCAB + "DecoderTest")) {

                Document document = LOADER.loadDocument(testCase.input, new DocumentLoaderOptions());

                assertNotNull(document);

                final Decoder decoder = CborLd.createDecoder(getDecoderConfig(testCase.config))
                        .loader(LOADER)
                        .compactArray(testCase.compactArrays)
                        .build();

                final JsonValue result = decoder.decode(((CborLdDocument) document).getByteArray());

                if (testCase.type.stream().noneMatch(o -> o.endsWith("PositiveEvaluationTest"))) {
                    fail("Expected error code [" + testCase.result + "].");
                    return;
                }

                assertNotNull(result);

                final JsonStructure expected = LOADER.loadDocument(testCase.result, new DocumentLoaderOptions()).getJsonContent().orElse(null);

                assertNotNull(expected);

                final boolean match = JsonLdComparison.equals(expected, result);

                if (!match) {
                    write(testCase, result, expected);
                }

                assertTrue(match, "The expected result does not match.");

            } else {
                fail("Unknown test execution method: " + testCase.type);
                return;
            }

        } catch (CborException e) {
            fail(e);

        } catch (ContextError e) {
            assertException(e.getCode().name(), e);

        } catch (EncoderError e) {
            assertException(e.getCode().name(), e);

        } catch (DecoderError e) {
            assertException(e.getCode().name(), e);

        } catch (JsonLdError e) {
            fail(e);
        }
    }

    final void assertException(final String code, Throwable e) {

        if (!isNegative()) {
            e.printStackTrace();
            fail("Unexpected error code [" + code + "]. " + e.getMessage());
            return;
        }

        // compare expected exception
        if (!Objects.equals(testCase.result.toASCIIString(), code)) {
            e.printStackTrace();
            fail("Expected error code [" + testCase.result.toASCIIString() + "] does not match [" + code + "].");
            return;
        }
    }

    final boolean isNegative() {
        return testCase.type.stream().anyMatch(o -> o.endsWith("NegativeEvaluationTest"));
    }

    public static void write(final CborLdTestCase testCase, final JsonValue result, final JsonStructure expected) {
        final StringWriter stringWriter = new StringWriter();

        try (final PrintWriter writer = new PrintWriter(stringWriter)) {
            writer.println("Test " + testCase.id.getFragment() + ": " + testCase.name);

            final JsonWriterFactory writerFactory = Json.createWriterFactory(Collections.singletonMap(JsonGenerator.PRETTY_PRINTING, true));

            if (expected != null) {
                write(writer, writerFactory, "Expected", expected);
                writer.println();
            }

            if (result != null) {
                write(writer, writerFactory, "Actual", result);
                writer.println();
            }
        }

        System.out.println(stringWriter.toString());
    }

    public static void write(final CborLdTestCase testCase, final byte[] result, final byte[] expected) {
        final StringWriter stringWriter = new StringWriter();

        try (final PrintWriter writer = new PrintWriter(stringWriter)) {
            writer.println("Test " + testCase.id.getFragment() + ": " + testCase.name);

            writer.println("Expected");
            writer.println("header = " + Hex.toString(expected, 3));

            CborWriter cborWriter = new CborWriter(writer);

            List<DataItem> decodedExpected = CborDecoder.decode(expected);
            assertNotNull(decodedExpected);

            if (decodedExpected != null) {
                cborWriter.write(decodedExpected);
                writer.println();
            }
            writer.println();

            writer.println("Actual");
            writer.println("header = " + Hex.toString(result, 3));
            List<DataItem> decodedResult = CborDecoder.decode(result);
            assertNotNull(decodedResult);

            if (decodedResult != null) {
                cborWriter.write(decodedResult);
                writer.println();
            }

        } catch (IOException e) {
            fail(e);

        } catch (CborException e) {
            fail(e);
        }

        System.out.println(stringWriter.toString());
    }

    static final void write(final PrintWriter writer, final JsonWriterFactory writerFactory, final String name, final JsonValue result) {

        writer.println(name + ":");

        final StringWriter out = new StringWriter();

        try (final JsonWriter jsonWriter = writerFactory.createWriter(out)) {
            jsonWriter.write(result);
        }

        writer.write(out.toString());
        writer.println();
    }

    static final boolean equalCborLdHeader(byte[] expected, byte[] actual) {
        if (expected == null || actual == null) {
            return actual == expected;
        }
        if (expected.length != actual.length) {
            return false;
        }
        for (int i = 0; i < 3; i++) {
            if (expected[i] != actual[i]) {
                return false;
            }
        }
        return true;
    }

    static final Encoder getEncoder(String name, boolean compactArrays) {
        if ("v5".equals(name)) {
            return compactArrays ? ENCODER_V05 : ENCODER_V05_NOCA;
        }
        if ("barcodes".equals(name)) {
            return ENCODER_V06;
        }
        return ENCODER_V1;
    }

    static final DecoderConfig getDecoderConfig(String name) {
        if ("v5".equals(name)) {
            return LegacyConfigV05.INSTANCE;
        }
        if ("barcodes".equals(name)) {
//            return  DecoderBuilder.of(CborLdVersion.V06).dictionary(Barcodes.DICTIONARY);
        }
        return DefaultConfig.INSTANCE;
    }
}
