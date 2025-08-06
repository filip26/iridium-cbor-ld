package com.apicatalog.cborld.registry;

import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.dictionary.DictionaryBuilder;

/**
 * Builder class for creating immutable {@link DocumentDictionary} instances.
 *
 * <p>
 * A {@code DocumentDictionaryBuilder} allows configuring:
 * </p>
 * <ul>
 * <li>Context URIs</li>
 * <li>Type dictionaries scoped by term</li>
 * <li>General-purpose URI mappings</li>
 * </ul>
 *
 * <p>
 * Each dictionary is identified by a unique integer code used in CBOR-LD
 * encoding to enable efficient semantic compression.
 * </p>
 */
public class DocumentDictionaryBuilder {

    protected final int code;
    protected final DictionaryBuilder contexts;
    protected final Map<String, DictionaryBuilder> types;
    protected final DictionaryBuilder uris;

    protected DocumentDictionaryBuilder(int code, DictionaryBuilder contexts, Map<String, DictionaryBuilder> types, final DictionaryBuilder uris) {
        this.code = code;
        this.contexts = contexts;
        this.types = types;
        this.uris = uris;
    }

    /**
     * Creates a new empty {@code DocumentDictionaryBuilder} with the given
     * dictionary code.
     *
     * @param code the unique dictionary code
     * @return a new builder instance
     */
    public static DocumentDictionaryBuilder create(int code) {
        return new DocumentDictionaryBuilder(code, DictionaryBuilder.create(), new HashMap<>(), DictionaryBuilder.create());
    }

    /**
     * Creates a new {@code DocumentDictionaryBuilder} initialized from an existing
     * dictionary.
     *
     * @param dictionary the dictionary to copy
     * @return a new builder instance pre-filled with existing mappings
     */
    public static DocumentDictionaryBuilder of(DocumentDictionary dictionary) {
        return new DocumentDictionaryBuilder(
                dictionary.code(),
                dictionary.contexts() != null
                        ? DictionaryBuilder.of(dictionary.contexts())
                        : DictionaryBuilder.create(),
                dictionary.types()
                        .entrySet()
                        .stream()
                        .map(e -> new AbstractMap.SimpleEntry<>(e.getKey(), DictionaryBuilder.of(e.getValue())))
                        .collect(Collectors.toMap(
                                Map.Entry::getKey,
                                Map.Entry::getValue)),
                dictionary.uris() != null
                        ? DictionaryBuilder.of(dictionary.uris())
                        : DictionaryBuilder.create());
    }

    /**
     * Builds and returns an immutable {@link DocumentDictionary} based on the
     * current state of the builder.
     *
     * @return a new {@code DocumentDictionary} instance
     */
    public DocumentDictionary build() {
        return new DocumentDictionaryImpl(
                code,
                contexts.build(),
                types.entrySet()
                        .stream()
                        .map(e -> new AbstractMap.SimpleEntry<>(e.getKey(), e.getValue().build()))
                        .collect(Collectors.toUnmodifiableMap(
                                Map.Entry::getKey,
                                Map.Entry::getValue)),
                uris.build());
    }

    /**
     * Adds a context URI mapping to the dictionary.
     *
     * @param value the context URI
     * @param code  the integer code to associate
     * @return this builder instance
     */
    public DocumentDictionaryBuilder context(String value, int code) {
        contexts.set(code, value);
        return this;
    }

    /**
     * Merges all context mappings from the given dictionary into this builder.
     *
     * @param dictionary the context dictionary to merge
     * @return this builder instance
     */
    public DocumentDictionaryBuilder context(Dictionary dictionary) {
        contexts.merge(dictionary);
        return this;
    }

    /**
     * Adds a value type mapping.
     *
     * @param name  the term or property name
     * @param code  the code to assign
     * @param value the type IRI to associate
     * @return this builder instance
     */
    public DocumentDictionaryBuilder type(String name, int code, String value) {
        DictionaryBuilder dictionary = types.get(name);
        if (dictionary == null) {
            dictionary = DictionaryBuilder.create();
            types.put(name, dictionary);
        }
        dictionary.set(code, value);
        return this;
    }

    /**
     * Replaces or sets the full dictionary for the given value type.
     *
     * @param name    the term or property name
     * @param builder the dictionary builder to assign
     * @return this builder instance
     */
    public DocumentDictionaryBuilder type(String name, DictionaryBuilder builder) {
        types.put(name, builder);
        return this;
    }

    /**
     * Adds a URI mapping to the dictionary.
     *
     * @param value the URI to map
     * @param code  the integer code to assign
     * @return this builder instance
     */
    public DocumentDictionaryBuilder uri(String value, int code) {
        uris.set(code, value);
        return this;
    }

    /**
     * Merges all URI mappings from the given dictionary into this builder.
     *
     * @param dictionary the URI dictionary to merge
     * @return this builder instance
     */
    public DocumentDictionaryBuilder uri(Dictionary dictionary) {
        uris.merge(dictionary);
        return this;
    }

    /**
     * Immutable implementation of the {@link DocumentDictionary} interface.
     */
    record DocumentDictionaryImpl(
            int code,
            Dictionary contexts,
            Map<String, Dictionary> types,
            Dictionary uris) implements DocumentDictionary {
    };
}
