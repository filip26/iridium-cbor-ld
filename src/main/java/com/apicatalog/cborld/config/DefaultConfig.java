package com.apicatalog.cborld.config;

import java.util.ArrayList;
import java.util.Collection;

import com.apicatalog.cborld.db.DbMappingProvider;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.decoder.value.ContextValueDecoder;
import com.apicatalog.cborld.decoder.value.DidKeyValueDecoder;
import com.apicatalog.cborld.decoder.value.MultibaseValueDecoder;
import com.apicatalog.cborld.decoder.value.TypeValueDecoder;
import com.apicatalog.cborld.decoder.value.UuidValueDecoder;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.decoder.value.VocabValueDecoder;
import com.apicatalog.cborld.decoder.value.XsdDateTimeValueDecoder;
import com.apicatalog.cborld.decoder.value.XsdDateValueDecoder;
import com.apicatalog.cborld.dictionary.ContextDictionary;
import com.apicatalog.cborld.encoder.EncoderConfig;
import com.apicatalog.cborld.encoder.value.ContextValueEncoder;
import com.apicatalog.cborld.encoder.value.DidKeyValueEncoder;
import com.apicatalog.cborld.encoder.value.MultibaseValueEncoder;
import com.apicatalog.cborld.encoder.value.TypeValueEncoder;
import com.apicatalog.cborld.encoder.value.UuidValueEncoder;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.encoder.value.VocabValueEncoder;
import com.apicatalog.cborld.encoder.value.XsdDateTimeValueEncoder;
import com.apicatalog.cborld.encoder.value.XsdDateValueEncoder;
import com.apicatalog.cborld.mapper.MappingProvider;

public final class DefaultConfig implements EncoderConfig, DecoderConfig {

    public static final DefaultConfig INSTANCE = new DefaultConfig();
    
    static final ContextDictionary CONTEXT_DICTIONARY = new ContextDictionary();
    
    static final Collection<ValueEncoder> VALUE_ENCODERS = new ArrayList<>();

    static {
        // term driven
        VALUE_ENCODERS.add(new ContextValueEncoder(CONTEXT_DICTIONARY));
//        VALUE_ENCODERS.add(new IdValueEncoder());
        
        // type driven
        VALUE_ENCODERS.add(new TypeValueEncoder());
        VALUE_ENCODERS.add(new XsdDateTimeValueEncoder());
        VALUE_ENCODERS.add(new XsdDateValueEncoder());
        VALUE_ENCODERS.add(new MultibaseValueEncoder());
        VALUE_ENCODERS.add(new VocabValueEncoder());
        
        // value driven
        VALUE_ENCODERS.add(new UuidValueEncoder());
        VALUE_ENCODERS.add(new DidKeyValueEncoder());
    }
    
    static final Collection<ValueDecoder> VALUE_DECODERS = new ArrayList<>();
    
    static {
        // term driven
        VALUE_DECODERS.add(new ContextValueDecoder(CONTEXT_DICTIONARY));
        
        // type driven
        VALUE_DECODERS.add(new TypeValueDecoder());
        VALUE_DECODERS.add(new XsdDateTimeValueDecoder());
        VALUE_DECODERS.add(new XsdDateValueDecoder());
        VALUE_DECODERS.add(new MultibaseValueDecoder());
        VALUE_DECODERS.add(new VocabValueDecoder());
        
        // value driven
        VALUE_DECODERS.add(new UuidValueDecoder());
        VALUE_DECODERS.add(new DidKeyValueDecoder());
    }

    static final boolean COMPACT_ARRAYS = true;
    
    static final MappingProvider MAPPING_PROVIDER = new DbMappingProvider(); 

    public static final boolean STATIC_CONTEXTS = true;

    @Override
    public boolean isCompactArrays() {
        return COMPACT_ARRAYS;
    }

    @Override
    public Collection<ValueEncoder> valueEncoders() {
        return VALUE_ENCODERS;
    }
    
//    @Override
//    public DictionaryAlgorithm dictonaryAlgorithm() {
//        return DictionaryAlgorithm.ProcessingOrderAppliedContexts;
//    }

    @Override
    public Collection<ValueDecoder> valueDecoders() {
        return VALUE_DECODERS;
    }
    
//    @Override
//    public MappingProvider provider() {
//        return MAPPING_PROVIDER;
//    }
    
    DefaultConfig() {}
}
