package com.apicatalog.cborld.encoder;

import java.util.Collection;

import com.apicatalog.cborld.config.BaseConfig;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.registry.DocumentDictionary;

/**
 * Configuration interface for controlling CBOR-LD encoding behavior.
 *
 * <p>
 * An {@code EncoderConfig} provides access to the core components required for
 * encoding JSON-LD documents into CBOR-LD, including:
 * </p>
 * <ul>
 * <li>A static {@link DocumentDictionary} for semantic compression</li>
 * <li>Custom {@link ValueEncoder}s for encoding values</li>
 * <li>An {@link EncoderMappingProvider} for dynamic type/term mappings</li>
 * </ul>
 *
 * <p>
 * This interface extends {@link BaseConfig}, inheriting common encoder settings
 * such as version and feature flags.
 * </p>
 */
public interface EncoderConfig extends BaseConfig {

    /**
     * Returns the static dictionary used for encoding known contexts, types, and
     * URIs.
     * <p>
     * The dictionary allows mapping common semantic values to compact CBOR codes,
     * enabling compression of JSON-LD terms.
     *
     * @return the active {@link DocumentDictionary}, or {@code null} if not set
     */
    DocumentDictionary dictionary();

    /**
     * Returns the collection of value encoders responsible for encoding specific
     * JSON-LD values.
     * <p>
     * These encoders can handle typed values, such as dates, numbers, or custom
     * data formats.
     *
     * @return a collection of {@link ValueEncoder} implementations
     */
    Collection<ValueEncoder> valueEncoders();

    /**
     * Returns the encoder mapping provider responsible for generating dynamic
     * term/type mappings.
     * <p>
     * This is used for cases where no static dictionary entry is available.
     *
     * @return the {@link EncoderMappingProvider}
     */
    EncoderMappingProvider encoderMapping();
}
