package com.apicatalog.cborld.context.mapping;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

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
    final Collection<ValueDecoder> valueDecoders;

    TypeMap typeMap;

    DecoderContextMapping(DocumentDictionary dictionary, Collection<ValueDecoder> valueDecoders) {
        this.dictionary = dictionary;
        this.valueDecoders = valueDecoders;
        this.termMap = CodeTermMap.create();
        this.typeKeyNameMap = new DefaultTypeKeyNameMapper();
        this.typeMap = null;
    }

    final DataItem decodeValue(final DataItem value, final String property) {

        var type = List.of(Keywords.TYPE);

        for (var decoder : valueDecoders) {
            try {
                var decoded = decoder.decode(
                        this,
                        value,
                        property,
                        Keywords.TYPE.equals(property) || typeKeyNameMap.isTypeKey(property)
                                ? type
                                : List.of());

                if (decoded != null) {
                    return new UnicodeString(decoded);
                }

            } catch (DecoderException e) {
                /* ignored */
            }
        }

        return value;
    }

    final DataItem encodeTerm(String term) {

        final Integer encodedTerm = termMap.getCode(term);

        if (encodedTerm != null) {
            return new UnsignedInteger(encodedTerm);
        }
        return new UnicodeString(term);
    }

    final String decodeTerm(final DataItem data) {

        if (data == null) {
            throw new IllegalArgumentException("The data parameter must not be null.");
        }

        switch (data.getMajorType()) {
        case UNICODE_STRING:
            return ((UnicodeString) data).getString();

        case UNSIGNED_INTEGER:
            return decodeTerm(((UnsignedInteger) data).getValue());

        default:
            return data.toString();
        }
    }

    final String decodeTerm(final BigInteger term) {

        if (term.mod(BigInteger.ONE.add(BigInteger.ONE)).equals(BigInteger.ZERO)) {
            String result = termMap.getValue(term.intValueExact());
            return result != null ? result : term.toString();
        }

        String result = termMap.getValue(term.subtract(BigInteger.ONE).intValueExact());

        return result != null ? result : term.toString();
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
