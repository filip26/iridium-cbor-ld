package com.apicatalog.cborld.registry;

import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import com.apicatalog.cborld.dictionary.Dictionary;

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
     * @return a {@link Dictionary} of context URI mappings
     */
    Dictionary contexts();

    /**
     * Returns a map of value-to-type dictionaries.
     * <p>
     * Each entry associates a term (e.g., a property name) with a
     * {@link Dictionary} of type aliases commonly used in the document.
     *
     * @return a map of term strings to corresponding type dictionaries
     */
    Map<String, Dictionary> types();

    /**
     * Returns the dictionary of general-purpose URIs used throughout the document.
     *
     * @return a {@link Dictionary} of URI mappings
     */
    Dictionary uris();

    /**
     * Creates a new empty {@code Builder} with the given dictionary code.
     *
     * @param code the unique dictionary code
     * @return a new builder instance
     */
    public static Builder newBuilder(int code) {
        return new Builder(
                code,
                Dictionary.newBuilder(),
                new HashMap<>(),
                Dictionary.newBuilder());
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
                        ? Dictionary.newBuilder(dictionary.contexts())
                        : Dictionary.newBuilder(),
                dictionary.types()
                        .entrySet()
                        .stream()
                        .map(e -> new AbstractMap.SimpleEntry<>(e.getKey(), Dictionary.newBuilder(e.getValue())))
                        .collect(Collectors.toUnmodifiableMap(
                                Map.Entry::getKey,
                                Map.Entry::getValue)),
                dictionary.uris() != null
                        ? Dictionary.newBuilder(dictionary.uris())
                        : Dictionary.newBuilder());
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
        private final Dictionary.Builder contexts;
        private final Map<String, Dictionary.Builder> types;
        private final Dictionary.Builder uris;

        private Builder(
                int code,
                Dictionary.Builder contexts,
                Map<String, Dictionary.Builder> types,
                Dictionary.Builder uris) {
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
         * Adds a context URI mapping to the dictionary.
         *
         * @param value the context URI
         * @param code  the integer code to associate
         * @return this builder instance
         */
        public Builder context(String value, int code) {
            contexts.set(code, value);
            return this;
        }

        /**
         * Merges all context mappings from the given dictionary into this builder.
         *
         * @param dictionary the context dictionary to merge
         * @return this builder instance
         */
        public Builder context(Dictionary dictionary) {
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
                dictionary = Dictionary.newBuilder();
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
        public Builder type(String name, Dictionary.Builder builder) {
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
        public Builder uri(String value, int code) {
            uris.set(code, value);
            return this;
        }

        /**
         * Merges all URI mappings from the given dictionary into this builder.
         *
         * @param dictionary the URI dictionary to merge
         * @return this builder instance
         */
        public Builder uri(Dictionary dictionary) {
            uris.merge(dictionary);
            return this;
        }

        /**
         * Immutable implementation of the {@link DocumentDictionary} interface.
         */
        private record DictionaryImpl(
                int code,
                Dictionary contexts,
                Map<String, Dictionary> types,
                Dictionary uris) implements DocumentDictionary {

            DictionaryImpl {
                contexts = contexts != null ? contexts : Dictionary.EMPTY; 
                types = types != null ? Map.copyOf(types) : Map.of();
                uris = uris != null ? uris : Dictionary.EMPTY;
            }
        }
    }
}
