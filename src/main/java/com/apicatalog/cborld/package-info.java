/**
 * Provides the public API for working with CBOR-LD (Concise Binary Object
 * Representation for Linked Data).
 *
 * <p>
 * Use static factory methods from {@link com.apicatalog.cborld.CborLd} to
 * create and configure CBOR-LD encoders and decoders.
 * </p>
 *
 * <pre>{@code
 * Encoder encoder = CborLd.createEncoder()
 *         .dictionary(myDictionary)
 *         .build();
 * }</pre>
 *
 * @see com.apicatalog.cborld.CborLd
 */
package com.apicatalog.cborld;
