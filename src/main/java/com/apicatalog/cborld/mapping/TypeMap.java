package com.apicatalog.cborld.mapping;

import java.util.Collection;

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
     * Returns the collection of types associated with the given term.
     *
     * @param term the JSON-LD term to look up
     * @return a collection of type IRIs associated with the term, or an empty
     *         collection if none are defined
     */
    Collection<String> getType(String term);

    /**
     * Returns a nested {@code TypeMap} associated with the given term, if the term
     * maps to a structured subgraph of type information.
     *
     * @param term the JSON-LD term
     * @return the corresponding {@code TypeMap}, or {@code null} if not present
     */
    TypeMap getMapping(String term);
}
