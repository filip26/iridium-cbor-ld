package com.apicatalog.cborld;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collections;
import java.util.Objects;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.json.cursor.JsonObjectCursor;
import com.apicatalog.json.cursor.jakarta.JakartaJsonCursor;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.json.JsonLdComparison;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.jsonld.loader.HttpLoader;
import com.apicatalog.jsonld.loader.SchemeRouter;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

public class CborLdTestRunnerJunit {

    private final CborLdTestCase testCase;

    public final static DocumentLoader LOADER =
            new UriBaseRewriter(
                    CborLdTestSuite.BASE,
                    "classpath:",
                    new SchemeRouter()
                    .set("http", HttpLoader.defaultInstance())
                    .set("https", HttpLoader.defaultInstance())
                    .set("classpath", new ClasspathLoader())
                );

    public CborLdTestRunnerJunit(CborLdTestCase testCase) {
        this.testCase = testCase;
    }

    public void execute() {

        assertNotNull(testCase.type);
        assertNotNull(testCase.input);

        try {
            if (testCase.type.contains(CborLdTestSuite.VOCAB + "EncoderTest")) {

        	Document document = LOADER.loadDocument(testCase.input, new DocumentLoaderOptions());
        	
        	JsonObject object = document.getJsonContent().orElseThrow(IllegalStateException::new).asJsonObject();
        	
        	JsonObjectCursor cursor = new JakartaJsonCursor(object, 100);
        	
        	byte[] bytes = CborLd.encoder(cursor).encode();

                if (testCase.type.stream().noneMatch(o -> o.endsWith("PositiveEvaluationTest"))) {
                    fail("Expected error code [" + testCase.result + "].");
                    return;
                }

        	assertNotNull(bytes);
        	
        	Document expected = LOADER.loadDocument(testCase.result, new DocumentLoaderOptions());
        	
        	assertNotNull(expected);
        	assertEquals(CborLdDocument.MEDIA_TYPE, expected.getContentType());
        	
        	assertArrayEquals(((CborLdDocument)expected).getByteArray(), bytes);
        	
            } else if (testCase.type.contains(CborLdTestSuite.VOCAB + "DecoderTest")) {

        	Document document = LOADER.loadDocument(testCase.input, new DocumentLoaderOptions());

        	assertNotNull(document);
        	
        	JsonValue result = CborLd.decoder(((CborLdDocument)document).getByteArray()).decode();

                if (testCase.type.stream().noneMatch(o -> o.endsWith("PositiveEvaluationTest"))) {
                    fail("Expected error code [" + testCase.result + "].");
                    return;
                }

        	assertNotNull(result);
        	
        	final JsonStructure expected = LOADER.loadDocument(testCase.result, new DocumentLoaderOptions()).getJsonContent().orElse(null);
        	
        	assertNotNull(expected);
        	
        	final boolean match =JsonLdComparison.equals(expected, result); 
        	
        	if (!match) {
        	    write(testCase, result, expected);
        	}
        	
        	assertTrue(match, "The expected result does not match.");

            } else {
                fail("Unknown test execution method: " + testCase.type);
                return;
            }

        } catch (EncoderError e) {
            
            if (testCase.type.stream().noneMatch(o -> o.endsWith("NegativeEvaluationTest"))) {
                fail("Unexpected error code [" + e.getCode() + "].");
                return;
            }

            assertEquals(testCase.result.toASCIIString(), e.getCode().name(), "Expected error code does not match");

        } catch (DecoderError e) {
            
            if (testCase.type.stream().noneMatch(o -> o.endsWith("NegativeEvaluationTest"))) {
                fail("Unexpected error code [" + e.getCode() + "]. " + e.getMessage());
                return;
            }

            assertEquals(testCase.result.toASCIIString(), e.getCode().name(), "Expected error code does not match");
             
        } catch (JsonLdError e) {
            e.printStackTrace();
            fail(e);
        }
    }

    final void assertException(final String code, Throwable e) {

        if (!isNegative()) {
            e.printStackTrace();
            fail(e);
            return;
        }

        if (!Objects.equals(testCase.result, code)) {
            e.printStackTrace();
        }

        // compare expected exception
        assertEquals(testCase.result, code);
    }

    final boolean isNegative() {
        return testCase.type.stream().anyMatch(o -> o.endsWith("NegativeEvaluationTest"));
    }

    public static void write(final CborLdTestCase testCase, final JsonValue result, final JsonStructure expected) {
        final StringWriter stringWriter = new StringWriter();

        try (final PrintWriter writer = new PrintWriter(stringWriter)) {
            writer.println("Test " + testCase.id + ": " + testCase.name);

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

    static final void write(final PrintWriter writer, final JsonWriterFactory writerFactory, final String name, final JsonValue result) {

        writer.println(name + ":");

        final StringWriter out = new StringWriter();

        try (final JsonWriter jsonWriter = writerFactory.createWriter(out)) {
            jsonWriter.write(result);
        }

        writer.write(out.toString());
        writer.println();
    }
}
