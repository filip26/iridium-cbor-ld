package com.apicatalog.cborld.registry;

import java.util.Map;

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
}
