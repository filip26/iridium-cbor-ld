package com.apicatalog.cborld.loader;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;

public class StaticContextLoader implements DocumentLoader {

    protected static Map<String, Document> staticCache = new HashMap<>();

    static {
        set("https://www.w3.org/2018/credentials/examples/v1", "2018-credentials-examples-v1.jsonld");
        set("https://www.w3.org/2018/credentials/v1", "2018-credentials-v1.jsonld");
        set("https://w3id.org/security/suites/ed25519-2020/v1", "security-suites-ed25519-2020-v1.jsonld");
        set("https://www.w3.org/ns/odrl.jsonld", "odrl.jsonld");
        set("https://www.w3.org/ns/did/v1", "did-v1.jsonld");
        set("https://www.w3.org/ns/credentials/v2", "credentials-v2.jsonld");
        set("https://w3id.org/vc-barcodes/v1", "vc-barcodes-v1.jsonld");
        set("https://w3id.org/security/data-integrity/v1", "data-integrity-v1.jsonld");
        set("https://w3id.org/security/multikey/v1", "multikey-v1.jsonld");
        set("https://w3id.org/security/data-integrity/v2", "data-integrity-v2.jsonld");
    }

    protected final DocumentLoader defaultLoader;

    public StaticContextLoader(DocumentLoader defaultLoader) {
        this.defaultLoader = defaultLoader;
    }

    @Override
    public Document loadDocument(URI url, DocumentLoaderOptions options) throws JsonLdError {
        if (staticCache.containsKey(url.toString())) {
            Document document = staticCache.get(url.toString());
            if (document != null) {
                return document;
            }
        }
        return defaultLoader.loadDocument(url, options);
    }

    public static void set(String url, Document document) {
        staticCache.put(url, document);
    }

    public static void set(String url, Class<?> clazz, String name) {
        set(url, get(clazz, name));
    }

    protected static JsonDocument get(Class<?> clazz, String name) {
        try (final InputStream is = clazz.getResourceAsStream(name)) {

            return JsonDocument.of(is);

        } catch (IOException | JsonLdError e) {
            e.printStackTrace();
        }
        return null;
    }

    private static void set(String url, String name) {
        set(url, get(StaticContextLoader.class, name));
    }
}
