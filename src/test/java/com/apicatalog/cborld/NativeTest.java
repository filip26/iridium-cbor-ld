package com.apicatalog.cborld;

import static org.junit.jupiter.api.Assertions.assertNotNull;

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
import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.StaticLoader;
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

    @Test
    void testMapEncode() throws EncoderException, DecoderException, CborException, IOException {

        var encoder = CborLd.newEncoder()
                .loader(loader)
//                .dictionary(dictionary)
                .build();

        Map<String, Object> jsonld = Map.of(
                "@context", "https://apicatalog/example-context.jsonld",
                "@type", "Person",
                "name", "Filip Kolařík",
                "project", "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK");

        var cborld = encoder.encode(jsonld);

        assertNotNull(cborld);

        var writer = new StringWriter();

        CborWriter cborWriter = new CborWriter(writer);

        final var decodedResult = CborDecoder.decode(cborld);

        cborWriter.write(decodedResult);

        System.out.println(writer.toString());

        var x = Decoder.newBuilder(Version.V1)
                .loader(loader)
                .dictionary(dictionary)
                .build()
                .decode(cborld);

        System.out.println(jsonld.toString().length());
        System.out.println(cborld.length);
        System.out.println(jsonld.toString());
        System.out.println(x);
    }

}
