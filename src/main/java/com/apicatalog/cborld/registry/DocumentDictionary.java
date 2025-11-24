package com.apicatalog.cborld.registry;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import com.apicatalog.cborld.mapping.TermMap;

/**
 * Represents a CBOR-LD document dictionary used for semantic compression.
 *
 * <p>
 * A {@code DocumentDictionary} defines a shared, static set of mappings for:
 * </p>
 * <ul>
 * <li>Context URIs</li>
 * <li>Commonly used types (organized by term)</li>
 * <li>Frequently referenced URIs</li>
 * </ul>
 * 
 * These mappings are identified by a unique dictionary code and are used during
 * encoding and decoding to reduce the size of CBOR-LD documents.
 */
public interface DocumentDictionary {

    /**
     * Returns the unique code identifying this dictionary.
     * <p>
     * This code is embedded in encoded CBOR-LD documents to indicate which
     * dictionary was used.
     *
     * @return the dictionary code
     */
    int code();

    /**
     * Returns the dictionary of known context URIs.
     *
     * @return a {@link TermMap} of context URI mappings
     */
    TermMap contexts();

    /**
     * Returns a map of value-to-type dictionaries.
     * <p>
     * Each entry associates a term (e.g., a property name) with a {@link TermMap}
     * of type aliases commonly used in the document.
     *
     * @return a map of term strings to corresponding type dictionaries
     */
    Map<String, TermMap> types();

    /**
     * Returns the dictionary of general-purpose URIs used throughout the document.
     *
     * @return a {@link TermMap} of URI mappings
     */
    TermMap uris();

    /**
     * Creates a new empty {@code Builder} with the given dictionary code.
     *
     * @param code the unique dictionary code
     * @return a new builder instance
     */
    public static Builder newBuilder(int code) {
        return new Builder(
                code,
                TermMap.newBuilder(),
                new HashMap<>(),
                TermMap.newBuilder());
    }

    /**
     * Creates a new {@code Builder} initialized from an existing dictionary.
     *
     * @param dictionary the dictionary to copy
     * @return a new builder instance pre-filled with existing mappings
     */
    public static Builder newBuilder(DocumentDictionary dictionary) {
        return new Builder(
                dictionary.code(),
                dictionary.contexts() != null
                        ? TermMap.newBuilder(dictionary.contexts())
                        : TermMap.newBuilder(),
                dictionary.types()
                        .entrySet()
                        .stream()
                        .map(e -> Map.entry(e.getKey(), TermMap.newBuilder(e.getValue())))
                        .collect(Collectors.toUnmodifiableMap(
                                Map.Entry::getKey,
                                Map.Entry::getValue)),
                dictionary.uris() != null
                        ? TermMap.newBuilder(dictionary.uris())
                        : TermMap.newBuilder());
    }

    /**
     * Builder class for creating immutable {@link DocumentDictionary} instances.
     *
     * <p>
     * A {@code Document.newBuilder()} allows configuring:
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
    public static class Builder {

        private final int code;
        private final TermMap.Builder contexts;
        private final Map<String, TermMap.Builder> types;
        private final TermMap.Builder uris;

        private Builder(
                int code,
                TermMap.Builder contexts,
                Map<String, TermMap.Builder> types,
                TermMap.Builder uris) {
            this.code = code;
            this.contexts = contexts;
            this.types = types;
            this.uris = uris;
        }

        /**
         * Builds and returns an immutable {@link DocumentDictionary} based on the
         * current state of the builder.
         *
         * @return a new {@code DocumentDictionary} instance
         */
        public DocumentDictionary build() {
            return new DictionaryImpl(
                    code,
                    contexts.build(),
                    types.entrySet()
                            .stream()
                            .map(e -> Map.entry(e.getKey(), e.getValue().build()))
                            .collect(Collectors.toUnmodifiableMap(
                                    Map.Entry::getKey,
                                    Map.Entry::getValue)),
                    uris.build());
        }

        /**
         * Adds a context term mapping to the dictionary.
         *
         * @param term the context term
         * @param code the integer code to associate
         * @return this builder instance
         */
        public Builder context(String term, int code) {
            contexts.set(code, term);
            return this;
        }

        /**
         * Merges all context mappings from the given dictionary into this builder.
         *
         * @param dictionary the context dictionary to merge
         * @return this builder instance
         */
        public Builder context(TermMap dictionary) {
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
        public Builder type(String name, int code, String value) {
            var dictionary = types.get(name);
            if (dictionary == null) {
                dictionary = TermMap.newBuilder();
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
        public Builder type(String name, TermMap.Builder builder) {
            types.put(name, builder);
            return this;
        }

        /**
         * Adds a URI mapping to the dictionary.
         *
         * @param uri  the URI to map
         * @param code the integer code to assign
         * @return this builder instance
         */
        public Builder uri(String uri, int code) {
            uris.set(code, uri);
            return this;
        }

        /**
         * Merges all URI mappings from the given dictionary into this builder.
         *
         * @param dictionary the URI dictionary to merge
         * @return this builder instance
         */
        public Builder uri(TermMap dictionary) {
            uris.merge(dictionary);
            return this;
        }

        /**
         * Immutable implementation of the {@link DocumentDictionary} interface.
         */
        private record DictionaryImpl(
                int code,
                TermMap contexts,
                Map<String, TermMap> types,
                TermMap uris) implements DocumentDictionary {

            DictionaryImpl {
                contexts = contexts != null ? contexts : TermMap.empty();
                types = types != null ? Map.copyOf(types) : Map.of();
                uris = uris != null ? uris : TermMap.empty();
            }
        }
    }
}
