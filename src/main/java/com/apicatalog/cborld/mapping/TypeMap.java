package com.apicatalog.cborld.mapping;

/**
 * Represents a mapping between JSON-LD terms and their associated types.
 *
 * <p>
 * A {@code TypeMap} provides type information used during encoding and
 * decoding, and supports nested mappings for compound or structured terms.
 * </p>
 */
public interface TypeMap {

    /**
     * Returns JSON-LD type associated with the given term.
     *
     * @param term the JSON-LD term to look up
     * @return JSON-LD type associated with the given term.
     */
    String getType(String term);

    /**
     * Returns a nested {@code TypeMap} associated with the given term, if the term
     * maps to a structured subgraph of type information.
     *
     * @param term the JSON-LD term
     * @return the corresponding {@code TypeMap}, or {@code null} if not present
     */
    TypeMap getMapping(String term);
}
