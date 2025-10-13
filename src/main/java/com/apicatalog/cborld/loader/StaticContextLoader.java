package com.apicatalog.cborld.loader;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;

/**
 * A {@link DocumentLoader} implementation that serves static, preloaded JSON-LD
 * context documents from the classpath, falling back to a default loader if not
 * found.
 *
 * <p>
 * This loader improves performance and reliability by avoiding network requests
 * for known context URIs bundled with the library. Contexts are loaded from
 * local resources based on hardcoded mappings.
 * </p>
 *
 * <p>
 * To register a new static context mapping, use {@link #set(String, Document)}
 * or {@link #set(String, Class, String)}.
 * </p>
 */
public class StaticContextLoader implements DocumentLoader {

    private static final Logger LOGGER = Logger.getLogger(StaticContextLoader.class.getName());

    protected static Map<String, Document> staticCache = new HashMap<>();

    static {
        set("https://www.w3.org/2018/credentials/examples/v1", "2018-credentials-examples-v1.jsonld");
        set("https://www.w3.org/2018/credentials/v1", "2018-credentials-v1.jsonld");
        set("https://w3id.org/security/suites/ed25519-2020/v1", "security-suites-ed25519-2020-v1.jsonld");
        set("https://www.w3.org/ns/odrl.jsonld", "odrl.jsonld");
        set("https://www.w3.org/ns/did/v1", "did-v1.jsonld");
        set("https://www.w3.org/ns/credentials/v2", "credentials-v2.jsonld");
        set("https://www.w3.org/ns/cid/v1", "cid-v1.jsonld");
        set("https://w3id.org/vc-barcodes/v1", "vc-barcodes-v1.jsonld");
        set("https://w3id.org/security/data-integrity/v1", "data-integrity-v1.jsonld");
        set("https://w3id.org/security/multikey/v1", "multikey-v1.jsonld");
        set("https://w3id.org/security/data-integrity/v2", "data-integrity-v2.jsonld");
        set("https://www.w3.org/ns/credentials/examples/v2", "credentials-examples-v2.jsonld");
        set("https://www.w3.org/ns/activitystreams", "activitystreams.jsonld");
    }

    protected final DocumentLoader defaultLoader;

    /**
     * Creates a new {@code StaticContextLoader} with a fallback
     * {@link DocumentLoader} for resolving contexts not found in the local static
     * cache.
     *
     * @param defaultLoader the fallback loader to use for unknown context URIs
     */
    public StaticContextLoader(DocumentLoader defaultLoader) {
        this.defaultLoader = defaultLoader;
    }

    /**
     * Loads a context document from the static cache if available. If not found,
     * delegates to the configured default loader.
     *
     * @param url     the context URI
     * @param options loading options
     * @return the loaded {@link Document}
     * @throws JsonLdError if loading fails
     */
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

    /**
     * Registers a static mapping between a context URI and a preloaded
     * {@link Document}.
     *
     * @param url      the URI to map
     * @param document the JSON-LD document to return when the URI is requested
     */
    public static void set(String url, Document document) {
        staticCache.put(url, document);
    }

    /**
     * Registers a static context mapping by loading a resource from the classpath.
     *
     * @param url   the URI to map
     * @param clazz the class whose classloader is used to locate the resource
     * @param name  the name of the classpath resource (e.g. "context.jsonld")
     */
    public static void set(String url, Class<?> clazz, String name) {
        set(url, get(clazz, name));
    }

    /**
     * Loads a {@link JsonDocument} from the classpath using the provided
     * classloader and resource name.
     *
     * @param clazz the class whose classloader will be used
     * @param name  the resource name (e.g. "vocab.jsonld")
     * @return the loaded {@code JsonDocument}, or {@code null} if loading failed
     */
    protected static JsonDocument get(Class<?> clazz, String name) {
        try (final InputStream is = clazz.getResourceAsStream(name)) {

            return JsonDocument.of(is);

        } catch (IOException | JsonLdError e) {
            LOGGER.log(Level.SEVERE, "An error [{0}] during loading static context [{1}]", new Object[] { e.getMessage(), name });
        }
        return null;
    }

    /**
     * Convenience method to register a static context mapping from a built-in
     * resource.
     *
     * @param url  the URI to map
     * @param name the resource name within the {@code StaticContextLoader}'s
     *             package
     */
    private static void set(String url, String name) {
        set(url, get(StaticContextLoader.class, name));
    }
}
