package com.apicatalog.cborld.dictionary;

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
public interface Dictionary extends Iterable<Map.Entry<String, Integer>> {

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
     * Creates a new, empty {@code DictionaryBuilder}.
     *
     * @return a new builder instance
     */
    public static Builder newBuilder() {
        return new Builder();
    }

    /**
     * Creates a new {@code DictionaryBuilder} from an existing {@link Dictionary}.
     *
     * <p>
     * If the given dictionary is an instance of {@link BidirectionalDictionary},
     * its internal structure is preserved. Otherwise, entries are merged.
     * </p>
     *
     * @param dictionary the dictionary to copy from
     * @return a new builder instance
     */
    public static Builder copyOf(Dictionary dictionary) {
        if (dictionary instanceof BidirectionalDictionary) {
            return new Builder((BidirectionalDictionary) dictionary);
        }
        return new Builder().merge(dictionary);
    }

    /**
     * A builder for constructing immutable {@link Dictionary} instances.
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
        private final Map<Integer, String> reverse;

        /**
         * Creates a new, empty {@code DictionaryBuilder}.
         */
        Builder() {
            this.reverse = new LinkedHashMap<>();
        }

        /**
         * Creates a new {@code DictionaryBuilder} initialized from an existing
         * {@link BidirectionalDictionary}.
         *
         * @param dictionary the source dictionary
         */
        Builder(BidirectionalDictionary dictionary) {
            this.reverse = new LinkedHashMap<>(dictionary.reverse());
        }

        /**
         * Merges entries from the given {@link Dictionary} into this builder.
         *
         * @param dictionary the dictionary to merge
         * @return this builder instance
         */
        public Builder merge(Dictionary dictionary) {
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
            reverse.put(code, value);
            return this;
        }

        /**
         * Adds or updates an entry mapping the given string value to the specified
         * code.
         *
         * @param value the string value
         * @param code  the corresponding integer code
         * @return this builder instance
         */
        public Builder set(String value, Integer code) {
            reverse.put(code, value);
            return this;
        }

        /**
         * Builds and returns an immutable {@link Dictionary} instance containing the
         * configured mappings.
         *
         * @return a new {@link Dictionary} instance
         */
        public Dictionary build() {
            return new BidirectionalDictionary(
                    reverse.entrySet()
                            .stream()
                            .collect(Collectors.toUnmodifiableMap(Entry::getValue, Entry::getKey)),
                    Map.copyOf(reverse));
        }
    }
}
