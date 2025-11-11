package com.apicatalog.cborld.debug;

import java.net.URI;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

/**
 * A debugging utility for inspecting the internals of a CBOR-LD encoding or
 * decoding process.
 *
 * <p>
 * {@code CborLdDebug} exposes detailed information about:
 * </p>
 * <ul>
 * <li>CBOR-LD version used</li>
 * <li>Associated {@link DocumentDictionary}</li>
 * <li>Dynamic term mappings via {@link Dictionary}</li>
 * <li>Encoded and decoded byte/JSON representations</li>
 * <li>Errors thrown during processing (if any)</li>
 * </ul>
 *
 * <p>
 * This class is used internally by {@code DebugEncoder} and
 * {@code DebugDecoder}, but can be used directly for detailed inspection or
 * testing.
 * </p>
 */
public class Debug {

    final DocumentLoader loader;
    final URI base;

    protected CborLdVersion version;
    protected DocumentDictionary dictionary;
    protected Mapping mapping;

    protected Object decoded;
    protected byte[] encoded;

    protected Exception error;

    /**
     * Constructs a new {@code CborLdDebug} instance.
     *
     * @param loader the document loader used during processing
     * @param base   the base URI used for context resolution
     */
    public Debug(DocumentLoader loader, URI base) {
        this.loader = loader;
        this.base = base;

        this.version = null;
        this.dictionary = null;
        this.mapping = null;

        this.decoded = null;
        this.encoded = null;

        this.error = null;
    }

    /**
     * Returns the CBOR-LD version used for encoding/decoding.
     *
     * @return the {@link CborLdVersion}, or {@code null} if unknown
     */
    public CborLdVersion version() {
        return version;
    }

    /**
     * Returns the dictionary used for compression.
     *
     * @return the active {@link DocumentDictionary}, or {@code null} if not
     *         available
     */
    public DocumentDictionary dictionary() {
        return dictionary;
    }

    /**
     * Returns the term dictionary (dynamic mapping).
     *
     * @return the {@link Dictionary} used for terms, or {@code null}
     */
    public Dictionary terms() {
        return mapping != null ? mapping.termMap() : null;
    }

    /**
     * Indicates if the document is a valid CBOR-LD encoding.
     *
     * @return {@code true} if version and dictionary are set
     */
    public boolean isCborLd() {
        return version != null && dictionary != null;
    }

    /**
     * Indicates if an error occurred during processing.
     *
     * @return {@code true} if an error is present
     */
    public boolean isError() {
        return error != null;
    }

    /**
     * Returns the exception that occurred during processing, if any.
     *
     * @return the exception or {@code null}
     */
    public Exception error() {
        return error;
    }

    /**
     * Returns the encoded CBOR-LD byte array.
     *
     * @return the encoded representation or {@code null}
     */
    public byte[] encoded() {
        return encoded;
    }

    /**
     * Returns the decoded JSON-LD document.
     *
     * @return the decoded value, or {@code null}
     */
    public Object decoded() {
        return decoded;
    }

    /**
     * Creates a human-readable representation of the current debug state.
     *
     * @return a {@link Map} summarizing version, dictionary, and term
     *         mappings
     */
    public Map<String, ?> dump() {
        return Map.of(
                "version", version != null
                        ? version.name()
                        : null,
                "dictionary", dictionary != null
                        ? dump(dictionary)
                        : Collections.emptyMap(),
                "terms", mapping != null && mapping.termMap() != null
                        ? dump(mapping.termMap())
                        : Collections.emptyMap());
    }

    static final Map<String, ?> dump(DocumentDictionary dictionary) {
        var result = new LinkedHashMap<>(Map.of(
                "code", dictionary.code(),
                "context", dictionary.contexts() != null
                        ? dump(dictionary.contexts())
                        : Collections.emptyMap()));

        if (dictionary.types() != null) {
            dictionary.types()
                    .entrySet()
                    .forEach(typeEntry -> result.put(typeEntry.getKey(), dump(typeEntry.getValue())));
        }

        if (dictionary.uris() != null) {
            result.put("uri", dump(dictionary.uris()));
        }

        return result;
    }

    static final Map<String, ?> dump(Dictionary dictionary) {
        var result = new LinkedHashMap<String, Object>();

        toStream(dictionary.iterator())
                .sorted((o1, o2) -> o1.getValue() - o2.getValue())
                .forEach(e -> result.put(e.getKey(), e.getValue()));

        return result;
    }

    static <T> Stream<T> toStream(Iterator<T> iterator) {
        return StreamSupport.stream(
                Spliterators.spliteratorUnknownSize(iterator, 0),
                false);
    }
}
