package com.apicatalog.cborld.dictionary;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

/**
 * A builder for constructing immutable {@link Dictionary} instances.
 *
 * <p>
 * Supports fluent creation of bidirectional mappings between string values
 * (e.g., terms or URIs) and integer codes for CBOR-LD compression.
 * </p>
 */
public class DictionaryBuilder {

    /**
     * Internal map from code to value (reverse direction).
     */
    protected final Map<Integer, String> reverse;

    /**
     * Creates a new, empty {@code DictionaryBuilder}.
     */
    protected DictionaryBuilder() {
        this.reverse = new LinkedHashMap<>();
    }

    /**
     * Creates a new {@code DictionaryBuilder} initialized from an existing
     * {@link BidirectionalDictionary}.
     *
     * @param dictionary the source dictionary
     */
    protected DictionaryBuilder(BidirectionalDictionary dictionary) {
        this.reverse = new LinkedHashMap<>(dictionary.reverse());
    }

    /**
     * Creates a new, empty {@code DictionaryBuilder}.
     *
     * @return a new builder instance
     */
    public static DictionaryBuilder create() {
        return new DictionaryBuilder();
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
    public static DictionaryBuilder of(Dictionary dictionary) {
        if (dictionary instanceof BidirectionalDictionary) {
            return new DictionaryBuilder((BidirectionalDictionary) dictionary);
        }
        return new DictionaryBuilder().merge(dictionary);
    }

    /**
     * Merges entries from the given {@link Dictionary} into this builder.
     *
     * @param dictionary the dictionary to merge
     * @return this builder instance
     */
    public DictionaryBuilder merge(Dictionary dictionary) {
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
    public DictionaryBuilder set(Integer code, String value) {
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
    public DictionaryBuilder set(String value, Integer code) {
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
                Collections.unmodifiableMap(reverse));
    }
}
