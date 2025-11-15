package com.apicatalog.cborld;

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
import com.apicatalog.cborld.debug.DebugDecoder;
import com.apicatalog.cborld.debug.DebugEncoder;
import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.jsonld.Comparison;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.loader.ClasspathLoader;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.StaticLoader;
import com.apicatalog.multibase.Multibase;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.cbor.CborParser;
import com.apicatalog.tree.io.jakarta.JakartaMaterializer;
import com.apicatalog.tree.io.jakarta.JakartaParser;
import com.apicatalog.tree.io.java.NativeAdapter;
import com.apicatalog.web.media.MediaType;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;
import jakarta.json.Json;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

class JunitRunner {

    private final TestCase testCase;

    static final TreeParser JSON_PARSER = new JakartaParser();
    static final CborParser CBOR_PARSER = new CborParser();

    // test loader using only local resources
    static final DocumentLoader TEST_LOADER = new UriBaseRewriter(
            TestSuite.BASE,
            "classpath:",
            new UriBaseRewriter(
                    "https://raw.githubusercontent.com/filip26/iridium-cbor-ld/main/src/test/resources/com/apicatalog/cborld/",
                    "classpath:",
                    ClasspathLoader
                            .newBuilder()
                            .base(CborLd.class)
                            .parser(MediaType.JSON, JSON_PARSER)
                            .parser(MediaType.JSON_LD, JSON_PARSER)
                            .parser(MediaType.CBOR, CBOR_PARSER)
                            .parser(MediaType.CBOR_LD, CBOR_PARSER)
                            .build()));

    static final DocumentLoader LOADER = StaticLoader
            .newBuilder()
            .parser(JSON_PARSER)
            // load bundled contexts
            .classpath(CborLd.CONTEXT_RESOURCES)
            // load from test resources
            .classpath("https://w3id.org/utopia/v2", "/com/apicatalog/cborld/context/utopia-v2-context.jsonld")
            .classpath("https://w3id.org/age/v1", "/com/apicatalog/cborld/context/age-v1-context.jsonld")
            .fallback(TEST_LOADER)
            .build();

    static final Encoder ENCODER_V1;
    static final Encoder ENCODER_V1_NOCP;
    static final Encoder ENCODER_UTOPIA_V1;
    static final Encoder ENCODER_UTOPIA_EXT_V1;
    static final Encoder ENCODER_UTOPIA_V06;
    static final Encoder ENCODER_V05;
    static final Encoder ENCODER_V05_NOCA;

    static final Decoder DECODER;
    static final Decoder DECODER_V05_NOCA;

    static final DebugDecoder DECODER_DEBUG;

    static {
        // several test instances reflecting various configurations
        ENCODER_V1 = CborLd.createEncoder()
                .loader(LOADER)
                .build();

        ENCODER_V1_NOCP = CborLd.createEncoder()
                .loader(LOADER)
                .dictionary(null)
                .build();

        ENCODER_UTOPIA_V1 = CborLd.createEncoder()
                .loader(LOADER)
                .dictionary(UtopiaBarcode.DICTIONARY)
                .build();

        ENCODER_UTOPIA_EXT_V1 = CborLd.createEncoder()
                .loader(LOADER)
                .dictionary(UtopiaBarcodeExtended.DICTIONARY)
                .build();

        ENCODER_UTOPIA_V06 = CborLd.createEncoder(CborLdVersion.V06)
                .loader(LOADER)
                .dictionary(UtopiaBarcode.DICTIONARY)
                .build();

        ENCODER_V05 = CborLd.createEncoder(CborLdVersion.V05)
                .loader(LOADER)
                .build();

        ENCODER_V05_NOCA = CborLd.createEncoder(CborLdVersion.V05)
                .loader(LOADER)
                .compactArray(false)
                .build();

        DECODER = CborLd.createDecoder(CborLdVersion.V1, CborLdVersion.V06, CborLdVersion.V05)
                .loader(LOADER)
                .dictionary(UtopiaBarcode.DICTIONARY)
                .dictionary(UtopiaBarcodeExtended.DICTIONARY)
                .dictionary(CborLdVersion.V06, UtopiaBarcode.DICTIONARY)
                .build();

        DECODER_V05_NOCA = CborLd.createDecoder(CborLdVersion.V05)
                .loader(LOADER)
                .compactArray(false)
                .build();

        DECODER_DEBUG = CborLd.createDecoder(CborLdVersion.V1, CborLdVersion.V06, CborLdVersion.V05)
                .loader(LOADER)
                .dictionary(UtopiaBarcode.DICTIONARY)
                .dictionary(UtopiaBarcodeExtended.DICTIONARY)
                .dictionary(CborLdVersion.V06, UtopiaBarcode.DICTIONARY)
                .debug();
    }

    public JunitRunner(TestCase testCase) {
        this.testCase = testCase;
    }

    public void execute() {

        assertNotNull(testCase.type);
        assertNotNull(testCase.input);

        try {
            if (testCase.type.contains(TestSuite.VOCAB + "EncoderTest")) {

                assumeFalse("t0025".equals(testCase.id.getFragment()));
                assumeFalse("t0026".equals(testCase.id.getFragment()));

                var document = LOADER.loadDocument(testCase.input, DocumentLoader.defaultOptions());

                var encoder = getEncoder(testCase.config, testCase.compactArrays);

                var encoded = encoder.encode(document.content());

                if (testCase.type.stream().noneMatch(o -> o.endsWith("PositiveEvaluationTest"))) {
                    fail("Expected error code [" + testCase.result + "].");
                    return;
                }

                assertNotNull(encoded);

                var expected = getResource(testCase.result.toString().substring(TestSuite.BASE.length()));

                assertNotNull(expected);

                final boolean match = equalCborLdHeader(expected, encoded)
                        && CborComparison.equals(expected, encoded);

                if (!match) {
                    write(testCase, encoded, expected);
                }

                assertTrue(match, "The expected result does not match.");

            } else if (testCase.type.contains(TestSuite.VOCAB + "DecoderTest")) {

                var input = getResource(testCase.input.toString().substring(TestSuite.BASE.length()));
                assertNotNull(input);

                final var decoder = getDecoder(testCase.config, testCase.compactArrays);

                final var result = decoder.decode(input);

                if (testCase.type.stream().noneMatch(o -> o.endsWith("PositiveEvaluationTest"))) {
                    fail("Expected error code [" + testCase.result + "].");
                    return;
                }

                assertNotNull(result);

                final var expected = LOADER.loadDocument(testCase.result, DocumentLoader.defaultOptions());
                assertNotNull(expected);

                var match = Comparison.equals(
                        expected.content().node(),
                        expected.content().adapter(),
                        result,
                        NativeAdapter.instance());

                if (!match) {
                    write(testCase, result, expected.content());
                }

                assertTrue(match, "The expected result does not match.");

            } else if (testCase.type.contains(TestSuite.VOCAB + "DebugEncoderTest")) {

                var document = LOADER.loadDocument(testCase.input, DocumentLoader.defaultOptions());

                var debug = getDebugEncoder(testCase.config);

                debug.encode(document.content());

                if (testCase.type.stream().noneMatch(o -> o.endsWith("PositiveEvaluationTest"))) {
                    fail("Expected error code [" + testCase.result + "].");
                    return;
                }

                var dump = debug.dump();

                var expected = LOADER.loadDocument(testCase.result, DocumentLoader.defaultOptions());
                assertNotNull(expected);

                // TODO use Jcs v2.0 with adapters
                var match = Comparison.equals(
                        expected.content().node(),
                        expected.content().adapter(),
                        dump,
                        NativeAdapter.instance());

                if (!match) {
                    write(testCase, dump, expected.content());
                }
                assertTrue(match, "The expected result does not match.");

            } else if (testCase.type.contains(TestSuite.VOCAB + "DebugDecoderTest")) {


                var input = getResource(testCase.input.toString().substring(TestSuite.BASE.length()));
                assertNotNull(input);

                var debug = DECODER_DEBUG;

                debug.decode(input);

                if (testCase.type.stream().noneMatch(o -> o.endsWith("PositiveEvaluationTest"))) {
                    fail("Expected error code [" + testCase.result + "].");
                    return;
                }

                var dump = debug.dump();

                var expected = LOADER.loadDocument(testCase.result, DocumentLoader.defaultOptions());
                assertNotNull(expected);

                // TODO use Jcs v2.0 with adapters
                var match = Comparison.equals(
                        expected.content().node(),
                        expected.content().adapter(),
                        dump,
                        NativeAdapter.instance());

                if (!match) {
                    write(testCase, dump, expected.content());
                }
                assertTrue(match, "The expected result does not match.");


            } else {
                fail("Unknown test execution method: " + testCase.type);
                return;
            }

        } catch (EncoderException e) {
            assertException(e.code().name(), e);

        } catch (DecoderException e) {
            assertException(e.code().name(), e);

        } catch (JsonLdException | CborException | IOException | TreeIOException e) {
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

    public static void write(final TestCase testCase, final Object result, final TreeIO expected) throws TreeIOException {
        final StringWriter stringWriter = new StringWriter();

        try (final PrintWriter writer = new PrintWriter(stringWriter)) {
            writer.println("Test " + testCase.id.getFragment() + ": " + testCase.name);

            final JsonWriterFactory writerFactory = Json.createWriterFactory(Collections.singletonMap(JsonGenerator.PRETTY_PRINTING, true));

            if (expected != null) {
                write(writer, writerFactory, "Expected", new JakartaMaterializer().node(expected));
                writer.println();
            }

            if (result != null) {
                write(writer, writerFactory, "Actual", new JakartaMaterializer().node(result, NativeAdapter.instance()));
                writer.println();
            }
        }

        System.out.println(stringWriter.toString());
    }

    public static void write(final TestCase testCase, final byte[] result, final byte[] expected) {
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
            writer.print("header = " + Hex.toString(result, 3));
            writer.println(", hex = " + Multibase.BASE_16_UPPER.encode(result).substring(1));

            final var decodedResult = CborDecoder.decode(result);

            cborWriter.write(decodedResult);
            writer.println();

        } catch (IOException | CborException e) {
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
        if ("v05".equals(name)) {
            return compactArrays ? ENCODER_V05 : ENCODER_V05_NOCA;
        }
        if ("v06utopia".equals(name)) {
            return ENCODER_UTOPIA_V06;
        }
        if ("v1utopia".equals(name)) {
            return ENCODER_UTOPIA_V1;
        }
        if ("v1utopiaext".equals(name)) {
            return ENCODER_UTOPIA_EXT_V1;
        }
        if ("v1nocp".equals(name)) {
            return ENCODER_V1_NOCP;
        }
        return ENCODER_V1;
    }

    static final DebugEncoder getDebugEncoder(String name) {
        if ("v1utopia".equals(name)) {
            return CborLd.createEncoder()
                    .loader(LOADER)
                    .dictionary(UtopiaBarcode.DICTIONARY)
                    .debug();
        }
        if ("v1utopiaext".equals(name)) {
            return CborLd.createEncoder()
                    .loader(LOADER)
                    .dictionary(UtopiaBarcodeExtended.DICTIONARY)
                    .debug();
        }
        return CborLd.createEncoder()
                .loader(LOADER)
                .debug();
    }

    static final Decoder getDecoder(String name, boolean compactArrays) {
        if ("v05".equals(name) && !compactArrays) {
            return DECODER_V05_NOCA;
        }
        return DECODER;
    }

    static final byte[] getResource(String name) throws IOException {
        try (var is = CborLd.class.getResourceAsStream(name)) {
            return is.readAllBytes();
        }
    }
}
