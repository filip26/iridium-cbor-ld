package com.apicatalog.cborld.mapping;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

/**
 * Represents a bidirectional mapping between strings (terms or URIs) and their
 * corresponding integer codes used for CBOR-LD compression.
 *
 * <p>
 * Implementations should provide fast lookup in both directions—string to code,
 * and code to string.
 * </p>
 */
public interface TermMap extends Iterable<Map.Entry<String, Integer>> {

    /**
     * Returns the integer code associated with the given string value.
     *
     * @param value the string term or URI
     * @return the corresponding code, or {@code null} if the value is not present
     */
    Integer getCode(String value);

    /**
     * Returns the string value associated with the given integer code.
     *
     * @param code the code to look up
     * @return the corresponding string value, or {@code null} if the code is not
     *         present
     */
    String getValue(Integer code);

    /**
     * Creates a new, empty {@code Builder}.
     *
     * @return a new builder instance
     */
    public static Builder newBuilder() {
        return new Builder(new LinkedHashMap<>());
    }

    /**
     * Creates a new {@code Builder} from an existing {@link TermMap}.
     *
     * <p>
     * If the given dictionary is an instance of {@link ImmutableTermMap}, its
     * internal structure is preserved. Otherwise, entries are merged.
     * </p>
     *
     * @param dictionary the dictionary to copy from
     * @return a new builder instance
     */
    public static Builder newBuilder(TermMap dictionary) {
        if (dictionary instanceof ImmutableTermMap map) {
            return newBuilder(map.index);
        }
        return new Builder(new LinkedHashMap<>()).merge(dictionary);
    }

    public static Builder newBuilder(Map<String, Integer> terms) {
        return new Builder(new LinkedHashMap<>(terms));
    }
    
    public static TermMap empty() {
        return ImmutableTermMap.EMPTY;
    }

    /**
     * A builder for constructing immutable {@link TermMap} instances.
     *
     * <p>
     * Supports fluent creation of bidirectional mappings between string values
     * (e.g., terms or URIs) and integer codes for CBOR-LD compression.
     * </p>
     */
    public static class Builder {

        /**
         * Internal map from code to value (reverse direction).
         */
        private final Map<String, Integer> terms;


        /**
         * Creates a new {@code DictionaryBuilder} initialized from an existing
         * dictionary.
         *
         * @param terms the source dictionary
         */
        private Builder(Map<String, Integer> terms) {
            this.terms = terms;
        }

        /**
         * Merges entries from the given {@link TermMap} into this builder.
         *
         * @param dictionary the dictionary to merge
         * @return this builder instance
         */
        public Builder merge(TermMap dictionary) {
            dictionary.forEach(entry -> set(entry.getKey(), entry.getValue()));
            return this;
        }

        /**
         * Adds or updates an entry mapping the given code to the specified string
         * value.
         *
         * @param code  the integer code
         * @param value the corresponding string value
         * @return this builder instance
         */
        public Builder set(Integer code, String value) {
            terms.put(value, code);
            return this;
        }

        /**
         * Adds or updates an entry mapping the given string value to the specified
         * code.
         *
         * @param term the term name
         * @param code the corresponding integer code
         * @return this builder instance
         */
        public Builder set(String term, Integer code) {
            terms.put(term, code);
            return this;
        }

        /**
         * Builds and returns an immutable {@link TermMap} instance containing the
         * configured mappings.
         *
         * @return a new {@link TermMap} instance
         */
        public TermMap build() {
            if (terms.isEmpty()) {
                return empty();
            }

            return new ImmutableTermMap(
                    Map.copyOf(terms),
                    terms.entrySet()
                            .stream()
                            .collect(Collectors.toUnmodifiableMap(Entry::getValue, Entry::getKey)));
        }
    }

    static class ImmutableTermMap implements TermMap {

        private static final TermMap EMPTY = new ImmutableTermMap(Map.of(), Map.of());
        
        private final Map<String, Integer> index;
        private final Map<Integer, String> reverse;

        private ImmutableTermMap(
                Map<String, Integer> index,
                Map<Integer, String> reverse) {
            this.index = index != null ? Map.copyOf(index) : Map.of();
            this.reverse = reverse != null ? Map.copyOf(reverse) : Map.of();
        }

        @Override
        public Integer getCode(String value) {
            return index.get(value);
        }

        @Override
        public String getValue(Integer code) {
            return reverse.get(code);
        }

        @Override
        public Iterator<Entry<String, Integer>> iterator() {
            return index.entrySet().iterator();
        }
    }
}
