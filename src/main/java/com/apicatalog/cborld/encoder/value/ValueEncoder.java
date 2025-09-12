package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.cborld.mapping.Mapping;

import co.nstant.in.cbor.model.DataItem;
import jakarta.json.JsonValue;

/**
 * Interface for encoding JSON-LD values into CBOR {@link DataItem}s.
 *
 * <p>
 * A {@code ValueEncoder} is responsible for serializing individual JSON values
 * based on their term and associated RDF types into compact CBOR
 * representations.
 * </p>
 *
 * <p>
 * Value encoders can support specific value types (e.g., strings, dates,
 * numbers, booleans) or custom encodings based on semantic type information
 * provided in the JSON-LD input.
 * </p>
 */
public interface ValueEncoder {

    /**
     * Encodes a JSON-LD value into a CBOR {@link DataItem}.
     *
     * @param mapping   the current {@link Mapping} context containing dictionaries
     *                  and type mappings
     * @param jsonValue the value to encode
     * @param term      the JSON-LD term (property) associated with the value
     * @param types     the set of semantic types associated with the term
     * @return a CBOR {@link DataItem} representing the encoded value
     * @throws EncoderException if encoding fails due to invalid structure,
     *                          unsupported type, or configuration issues
     */
    DataItem encode(Mapping mapping, String value, String term, Collection<String> types) throws EncoderException;

}
