package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;

/**
 * Interface for encoding a value into CBOR {@link DataItem}.
 *
 * <p>
 * A {@code ValueEncoder} is responsible for serializing individual values based
 * on their location and associated RDF types into compact CBOR representations.
 * </p>
 */
@FunctionalInterface
public interface ValueEncoder {

    /**
     * Encodes a value into a CBOR {@link DataItem}.
     *
     * @param mapping the current {@link Mapping} context containing dictionaries
     *                and type mappings
     * @param value   the value to encode, never <code>null</code>
     * @param term    the JSON-LD term (property) / RDF predicate associated with
     *                the value
     * @param types   the set of RDF types associated with the term
     * @return a CBOR {@link DataItem} representing the encoded value
     * @throws EncoderException if encoding fails due to invalid structure,
     *                          unsupported type, or configuration issues
     */
    DataItem encode(Mapping mapping, String value, String term, Collection<String> types) throws EncoderException;
}
