package com.apicatalog.cborld.db;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.mapper.Mapping;
import com.apicatalog.cborld.mapper.TypeMap;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

class DbDecoderMapping implements Mapping {

    private final CodeTermMap dictionary;
    private final TypeKeyNameMapper typeKeyNameMap;
    private TypeMap typeMap;
    
    private Collection<ValueDecoder> valueDecoders;
    
    DbDecoderMapping() {
        this.dictionary = CodeTermMap.create();
        this.typeKeyNameMap = new DefaultTypeKeyNameMapper();
        this.typeMap = null;
    }
    
    void valueDecoders(Collection<ValueDecoder> valueDecoders) {
        this.valueDecoders = valueDecoders;
    }
    
    final DataItem decodeValue(final DataItem value, String term, Collection<String> path) {

        //FIXME
        Collection<String> TYPE = Arrays.asList(Keywords.TYPE); 
        
        for (final ValueDecoder decoder : valueDecoders) {
            try {
                final JsonValue decoded = decoder.decode(dictionary(), value, term, 
                        typeKeyNameMap.isTypeKey(term, path)
                        ? TYPE
                        : Collections.emptySet()
                        );
                
                if (decoded == null) {
                    continue;
                }
                
                if (JsonUtils.isString(decoded)) {
                    return new UnicodeString(((JsonString)decoded).getString());
                }
                
            } catch (DecoderError e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            
        }
        
        return value;
    }
    
    final DataItem encodeKey(String key) {
        
        final BigInteger encodedProperty = dictionary.getCode(key);
        
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
            return decodeKey(((UnicodeString)data).getString());
    
        case UNSIGNED_INTEGER:
            return decodeKey(((UnsignedInteger)data).getValue());

        default:
            return data.toString();
        }
    }

    final String decodeKey(String key) {
        return key;
    }

    final String decodeKey(BigInteger key) {
        
        if (key.mod(BigInteger.ONE.add(BigInteger.ONE)).equals(BigInteger.ZERO)) {
            String result = dictionary.getValue(key);
            return result != null ? result : key.toString();
        }        
    
        String result = dictionary.getValue(key.subtract(BigInteger.ONE));

        return result != null ? result : key.toString();
    }

    
    @Override
    public Dictionary dictionary() {
        return dictionary;
    }

    @Override
    public TypeMap typeMap() {
        return typeMap;
    }
    
    public void typeMap(TypeMap typeMap) {
        this.typeMap = typeMap;
    }
    
    public void add(Collection<String> keySet) {
        dictionary.add(keySet);
    }
    
    public TypeKeyNameMapper typeKeyNameMap() {
        return typeKeyNameMap;
    }
}
