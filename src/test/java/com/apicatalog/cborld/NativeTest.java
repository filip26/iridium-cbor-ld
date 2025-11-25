package com.apicatalog.cborld;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.apicatalog.cbor.CborWriter;
import com.apicatalog.cborld.CborLd.Version;
import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.Comparison;
import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.StaticLoader;
import com.apicatalog.multibase.Multibase;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.java.NativeAdapter;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;

public class NativeTest {

    static DocumentLoader loader = StaticLoader.newBuilder()
            .document(
                    "https://apicatalog/example-context.jsonld",
                    Document.of(new TreeIO(Map.of(
                            "@context", Map.of(
                                    "name", "http://xmlns.com/foaf/0.1/name",
                                    "project", Map.of(
                                            "@id", "http://xmlns.com/foaf/0.1/project",
                                            "@type", "@id"),
                                    "Person", "http://xmlns.com/foaf/0.1/Person")),
                            NativeAdapter.instance())))
            .build();

    static DocumentDictionary dictionary = DocumentDictionary.newBuilder(123)
            .context("https://apicatalog/example-context.jsonld", 1)
            .uri("did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK", 1)
            .build();

    static Map<String, Object> jsonld = Map.of(
            "@context", "https://apicatalog/example-context.jsonld",
            "@type", "Person",
            "name", "Filip Kolařík",
            "project", "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK");

    static String cborld = "FD9CB1D82187BA4000102186418666F46696C6970204B6F6C61C599C3AD6B186801";

    @Test
    void testMapEncode() throws EncoderException, DecoderException, CborException, IOException {

        var encoder = CborLd.newEncoder()
                .loader(loader)
                .dictionary(dictionary)
                .build();

        var result = encoder.encode(jsonld);

        assertNotNull(result);

        var match = cborld.equals(Multibase.BASE_16_UPPER.encode(result));

        if (!match) {
            var writer = new StringWriter();
            writer.write("Expected:\n");
            
            CborWriter cborWriter = new CborWriter(writer);
            cborWriter.write(CborDecoder.decode(Multibase.BASE_16_UPPER.decode(cborld)));
            writer.flush();
            writer.write("\n\nActual:\n");
            cborWriter.write(CborDecoder.decode(result));

            System.out.println(writer.toString());
        }

        assertTrue(match);
    }

    @Test
    void testMapDecode() throws EncoderException, DecoderException, CborException, IOException {

        var decoder = Decoder.newBuilder(Version.V1)
                .loader(loader)
                .dictionary(dictionary)
                .build();

        var result = decoder.decode(Multibase.BASE_16_UPPER.decode(cborld));

        assertNotNull(result);

        var match = Comparison.equals(jsonld, result, NativeAdapter.instance());

        assertTrue(match);
    }
}
