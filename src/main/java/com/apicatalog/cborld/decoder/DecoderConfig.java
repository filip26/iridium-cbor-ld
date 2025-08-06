package com.apicatalog.cborld.decoder;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.cborld.config.BaseConfig;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.registry.DocumentDictionary;

/**
 * Configuration interface for controlling CBOR-LD decoding behavior.
 *
 * <p>
 * A {@code DecoderConfig} provides access to the core components used during
 * decoding, including registered dictionaries, value decoders, and term/type
 * mapping.
 * </p>
 *
 * <p>
 * This interface extends {@link BaseConfig}, inheriting general configuration
 * such as versioning and feature toggles.
 * </p>
 */
public interface DecoderConfig extends BaseConfig {

    /**
     * Returns the registry of known document dictionaries available to the decoder.
     * <p>
     * Each dictionary is indexed by its unique integer code, which must match the
     * code embedded in the CBOR-LD document.
     *
     * @return a map of dictionary code to {@link DocumentDictionary}
     */
    Map<Integer, DocumentDictionary> registry();

    /**
     * Returns the list of value decoders used to process JSON-LD values during
     * decoding.
     * <p>
     * Value decoders may perform custom deserialization or type conversion for
     * specific value patterns.
     *
     * @return a collection of {@link ValueDecoder} instances
     */
    Collection<ValueDecoder> valueDecoders();

    /**
     * Returns the mapping provider responsible for generating term and type
     * mappings used in dynamic compression/decompression.
     *
     * @return the {@link DecoderMappingProvider}
     */
    DecoderMappingProvider decoderMapping();

}
