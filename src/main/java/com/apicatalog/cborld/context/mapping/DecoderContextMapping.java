package com.apicatalog.cborld.context.mapping;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.mapping.TypeKeyNameMapper;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

class DecoderContextMapping implements Mapping {

    private final DocumentDictionary dictionary;
    private final CodeTermMap terms;
    private final TypeKeyNameMapper typeKeyNameMap;
    private TypeMap typeMap;

    private final Collection<ValueDecoder> valueDecoders;

    DecoderContextMapping(DocumentDictionary dictionary, Collection<ValueDecoder> valueDecoders) {
        this.dictionary = dictionary;
        this.valueDecoders = valueDecoders;
        this.terms = CodeTermMap.create();
        this.typeKeyNameMap = new DefaultTypeKeyNameMapper();
        this.typeMap = null;
    }

    final DataItem decodeValue(final DataItem value, String term) {

        Collection<String> TYPE = Arrays.asList(Keywords.TYPE);

        for (final ValueDecoder decoder : valueDecoders) {
            try {
                final JsonValue decoded = decoder.decode(this, value, term,
                        typeKeyNameMap.isTypeKey(term)
                                ? TYPE
                                : Collections.emptySet());

                if (decoded == null) {
                    continue;
                }

                if (JsonUtils.isString(decoded)) {
                    return new UnicodeString(((JsonString) decoded).getString());
                }

            } catch (DecoderError e) {
                /* ignored */
            }
        }

        return value;
    }

    final DataItem encodeKey(String key) {

        final Integer encodedProperty = terms.getCode(key);

        if (encodedProperty != null) {
            return new UnsignedInteger(encodedProperty);
        }
        return new UnicodeString(key);
    }

    final String decodeKey(final DataItem data) {

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
            String result = terms.getValue(key.intValueExact());
            return result != null ? result : key.toString();
        }

        String result = terms.getValue(key.subtract(BigInteger.ONE).intValueExact());

        return result != null ? result : key.toString();
    }

    @Override
    public Dictionary termMap() {
        return terms;
    }

    @Override
    public TypeMap typeMap() {
        return typeMap;
    }

    public void typeMap(TypeMap typeMap) {
        this.typeMap = typeMap;
    }

    public void add(Collection<String> keySet) {
        terms.add(keySet);
    }

    public TypeKeyNameMapper typeKeyNameMap() {
        return typeKeyNameMap;
    }

    @Override
    public DocumentDictionary dictionary() {
        return dictionary;
    }
}
