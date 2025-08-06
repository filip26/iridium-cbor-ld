package com.apicatalog.cborld.dictionary;

import java.util.Map;

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
}
