package com.apicatalog.cborld;

import com.apicatalog.jsonld.loader.ClasspathLoader;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.StaticLoader;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.jakarta.JakartaParser;

public class TestManifest {

    static final TreeParser JSON_PARSER = new JakartaParser();

    // test loader using only local resources
    static final DocumentLoader TEST_LOADER = new UriBaseRewriter(
            CborLdTest.BASE,
            "classpath:",
            new UriBaseRewriter(
                    "https://raw.githubusercontent.com/filip26/iridium-cbor-ld/main/src/test/resources/com/apicatalog/cborld/",
                    "classpath:",
                    new ClasspathLoader(JSON_PARSER)));

    public static final DocumentLoader LOADER = StaticLoader
            .newBuilder()
            .parser(JSON_PARSER)
            // load bundled contexts
            .classpath(CborLd.CONTEXT_RESOURCES)
            // load from test resources
            .classpath("https://w3id.org/utopia/v2", "/com/apicatalog/cborld/context/utopia-v2-context.jsonld")
            .classpath("https://w3id.org/age/v1", "/com/apicatalog/cborld/context/age-v1-context.jsonld")
            .fallback(TEST_LOADER)
            .build();

}
