package com.apicatalog.cborld.mapping;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.registry.DocumentDictionary;

/**
 * Represents a mapping configuration used during CBOR-LD encoding or decoding.
 *
 * <p>
 * A {@code Mapping} provides access to both static and dynamic dictionaries:
 * </p>
 * <ul>
 * <li>A {@link DocumentDictionary} containing predefined, static mappings</li>
 * <li>A dynamic {@link Dictionary} for terms discovered at runtime</li>
 * <li>A dynamic {@link TypeMap} for managing type-related mappings</li>
 * </ul>
 */
public interface Mapping {

    static Mapping EMPTY = new Mapping() {
        @Override
        public TypeMap typeMap() {
            return null;
        }

        @Override
        public Dictionary termMap() {
            return null;
        }

        @Override
        public DocumentDictionary dictionary() {
            return null;
        }
    };

    /**
     * Returns the static {@link DocumentDictionary} used as a predefined reference
     * for compression or decompression.
     *
     * @return the associated {@code DocumentDictionary}
     */
    DocumentDictionary dictionary();

    /**
     * Returns the dynamic {@link Dictionary} used for term-to-code mappings
     * discovered during processing.
     *
     * @return the dynamic term map
     */
    Dictionary termMap();

    /**
     * Returns the dynamic {@link TypeMap} for managing type-specific mappings
     * encountered during encoding or decoding.
     *
     * @return the dynamic type map
     */
    TypeMap typeMap();
}
