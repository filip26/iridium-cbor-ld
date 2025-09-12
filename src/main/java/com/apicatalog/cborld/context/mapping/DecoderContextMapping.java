package com.apicatalog.cborld.context.mapping;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.mapping.TypeKeyNameMapper;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

class DecoderContextMapping implements Mapping {

    final DocumentDictionary dictionary;
    final CodeTermMap termMap;
    final TypeKeyNameMapper typeKeyNameMap;
    TypeMap typeMap;

    private final Collection<ValueDecoder> valueDecoders;

    DecoderContextMapping(DocumentDictionary dictionary, Collection<ValueDecoder> valueDecoders) {
        this.dictionary = dictionary;
        this.valueDecoders = valueDecoders;
        this.termMap = CodeTermMap.create();
        this.typeKeyNameMap = new DefaultTypeKeyNameMapper();
        this.typeMap = null;
    }

    final DataItem decodeValue(final DataItem value, String term) {

        var type = Arrays.asList(Keywords.TYPE);

        for (final ValueDecoder decoder : valueDecoders) {
            try {
                final String decoded = decoder.decode(this, value, term,
                        typeKeyNameMap.isTypeKey(term)
                                ? type
                                : Collections.emptySet());

                if (decoded == null) {
                    continue;
                }

                return new UnicodeString(decoded);

            } catch (DecoderException e) {
                /* ignored */
            }
        }

        return value;
    }

    final DataItem encodeTerm(String key) {

        final Integer encodedProperty = termMap.getCode(key);

        if (encodedProperty != null) {
            return new UnsignedInteger(encodedProperty);
        }
        return new UnicodeString(key);
    }

    final String decodeTerm(final DataItem data) {

        if (data == null) {
            throw new IllegalArgumentException("The data parameter must not be null.");
        }

        switch (data.getMajorType()) {
        case UNICODE_STRING:
            return ((UnicodeString) data).getString();

        case UNSIGNED_INTEGER:
            return decodeKey(((UnsignedInteger) data).getValue());

        default:
            return data.toString();
        }
    }

    final String decodeKey(final BigInteger key) {

        if (key.mod(BigInteger.ONE.add(BigInteger.ONE)).equals(BigInteger.ZERO)) {
            String result = termMap.getValue(key.intValueExact());
            return result != null ? result : key.toString();
        }

        String result = termMap.getValue(key.subtract(BigInteger.ONE).intValueExact());

        return result != null ? result : key.toString();
    }

    @Override
    public Dictionary termMap() {
        return termMap;
    }

    @Override
    public TypeMap typeMap() {
        return typeMap;
    }

    public void typeMap(TypeMap typeMap) {
        this.typeMap = typeMap;
    }

    public void add(Collection<String> keySet) {
        termMap.add(keySet);
    }

    public TypeKeyNameMapper typeKeyNameMap() {
        return typeKeyNameMap;
    }

    @Override
    public DocumentDictionary dictionary() {
        return dictionary;
    }
}
