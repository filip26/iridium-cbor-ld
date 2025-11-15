package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;

/**
 * Interface for decoding CBOR {@link DataItem} values into JSON-LD values.
 *
 * <p>
 * A {@code ValueDecoder} is responsible for interpreting a CBOR value based on
 * the current term and its associated types, using mapping context to guide the
 * decoding.
 * </p>
 *
 * <p>
 * Implementations may support type-specific decoding (e.g., date strings,
 * booleans, identifiers), and are typically registered as part of the decoder
 * configuration.
 * </p>
 */
@FunctionalInterface
public interface ValueDecoder {

    /**
     * Decodes a CBOR {@link DataItem} into a corresponding string value, using the
     * provided context and type information.
     *
     * @param mapping the current {@link Mapping} context containing dictionaries
     *                and type maps
     * @param value   the CBOR value to decode
     * @param term    the JSON-LD term (property) associated with the value
     * @param types   a collection of semantic type IRIs associated with the term
     * @return the decoded value
     * @throws DecoderException if decoding fails due to unsupported structure, type
     *                          mismatch, or malformed data
     */
    String decode(Mapping mapping, DataItem value, String term, Collection<String> types) throws DecoderException;
}
