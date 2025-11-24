package com.apicatalog.cborld.config;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.CborLd.Version;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.decoder.DecoderMappingProvider;
import com.apicatalog.cborld.decoder.value.ContextValueDecoder;
import com.apicatalog.cborld.decoder.value.CustomTypeValueDecoder;
import com.apicatalog.cborld.decoder.value.DidKeyValueDecoder;
import com.apicatalog.cborld.decoder.value.IdValueDecoder;
import com.apicatalog.cborld.decoder.value.MultibaseValueDecoder;
import com.apicatalog.cborld.decoder.value.TypeValueDecoder;
import com.apicatalog.cborld.decoder.value.UuidValueDecoder;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.decoder.value.VocabValueDecoder;
import com.apicatalog.cborld.decoder.value.XsdDateTimeValueDecoder;
import com.apicatalog.cborld.decoder.value.XsdDateValueDecoder;
import com.apicatalog.cborld.encoder.EncoderConfig;
import com.apicatalog.cborld.encoder.EncoderMappingProvider;
import com.apicatalog.cborld.encoder.value.ContextValueEncoder;
import com.apicatalog.cborld.encoder.value.CustomTypeValueEncoder;
import com.apicatalog.cborld.encoder.value.DidKeyValueEncoder;
import com.apicatalog.cborld.encoder.value.IdValueEncoder;
import com.apicatalog.cborld.encoder.value.MultibaseValueEncoder;
import com.apicatalog.cborld.encoder.value.TypeValueEncoder;
import com.apicatalog.cborld.encoder.value.UuidValueEncoder;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.encoder.value.VocabValueEncoder;
import com.apicatalog.cborld.encoder.value.XsdDateTimeValueEncoder;
import com.apicatalog.cborld.encoder.value.XsdDateValueEncoder;
import com.apicatalog.cborld.mapping.context.ContextMappingProvider;
import com.apicatalog.cborld.registry.DefaultDocumentDictionary;
import com.apicatalog.cborld.registry.DocumentDictionary;

public class ConfigV1 implements EncoderConfig, DecoderConfig {

    public static final ConfigV1 INSTANCE = new ConfigV1();

    static final ContextMappingProvider MAPPING = new ContextMappingProvider();

    static final Collection<ValueEncoder> VALUE_ENCODERS;

    static {
        
        Collection<ValueEncoder> valueEncoders = new ArrayList<>();
        
        // property driven
        valueEncoders.add(new ContextValueEncoder());

        // type driven
        valueEncoders.add(new IdValueEncoder());
        valueEncoders.add(new TypeValueEncoder());
        valueEncoders.add(new XsdDateTimeValueEncoder());
        valueEncoders.add(new XsdDateValueEncoder());
        valueEncoders.add(new MultibaseValueEncoder());
        valueEncoders.add(new VocabValueEncoder());
        valueEncoders.add(new CustomTypeValueEncoder());

        // value driven
        valueEncoders.add(new UuidValueEncoder());
        valueEncoders.add(new DidKeyValueEncoder());
        
        VALUE_ENCODERS = Collections.unmodifiableCollection(valueEncoders);
    }

    static final Collection<ValueDecoder> VALUE_DECODERS = new ArrayList<>();

    static {
        // property driven
        VALUE_DECODERS.add(new ContextValueDecoder());

        // type driven
        VALUE_DECODERS.add(new IdValueDecoder());
        VALUE_DECODERS.add(new TypeValueDecoder());
        VALUE_DECODERS.add(new XsdDateTimeValueDecoder());
        VALUE_DECODERS.add(new XsdDateValueDecoder());
        VALUE_DECODERS.add(new MultibaseValueDecoder());
        VALUE_DECODERS.add(new VocabValueDecoder());
        VALUE_DECODERS.add(new CustomTypeValueDecoder());

        // value driven
        VALUE_DECODERS.add(new UuidValueDecoder());
        VALUE_DECODERS.add(new DidKeyValueDecoder());
    }

    public static final boolean COMPACT_ARRAYS = false;

    static final Map<Integer, DocumentDictionary> REGISTRY;

    static final DocumentDictionary DICTIONARY = new DefaultDocumentDictionary(1);
//FIXME    static final DocumentDictionary DICTIONARY = DocumentDictionary.newBuilder(1).build();
    
    static {
        REGISTRY = new HashMap<>();
        REGISTRY.put(DICTIONARY.code(), DICTIONARY);
    }
        
     @Override
    public Collection<ValueEncoder> valueEncoders() {
        return VALUE_ENCODERS;
    }

    @Override
    public Collection<ValueDecoder> valueDecoders() {
        return VALUE_DECODERS;
    }

    @Override
    public DecoderMappingProvider decoderMapping() {
        return MAPPING;
    }

    @Override
    public EncoderMappingProvider encoderMapping() {
        return MAPPING;
    }

    @Override
    public Map<Integer, DocumentDictionary> registry() {
        return REGISTRY;
    }

    @Override
    public DocumentDictionary dictionary() {
        return DICTIONARY;
    }

    @Override
    public Version version() {
        return Version.V1;
    }
    
    @Override
    public boolean isCompactArrays() {
        return COMPACT_ARRAYS;
    }
}
