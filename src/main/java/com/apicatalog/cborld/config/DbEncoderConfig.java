package com.apicatalog.cborld.config;

import java.util.ArrayList;
import java.util.Collection;

import com.apicatalog.cborld.dictionary.ContextDictionary;
import com.apicatalog.cborld.encoder.EncoderConfigration;
import com.apicatalog.cborld.encoder.value.ContextValueEncoder;
import com.apicatalog.cborld.encoder.value.DidKeyValueEncoder;
import com.apicatalog.cborld.encoder.value.MultibaseValueEncoder;
import com.apicatalog.cborld.encoder.value.TypeValueEncoder;
import com.apicatalog.cborld.encoder.value.UuidValueEncoder;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.encoder.value.VocabValueEncoder;
import com.apicatalog.cborld.encoder.value.XsdDateTimeValueEncoder;
import com.apicatalog.cborld.encoder.value.XsdDateValueEncoder;

public final class DbEncoderConfig implements EncoderConfigration {

    public static final Collection<ValueEncoder> VALUE_ENCODERS = new ArrayList<>();

    static {
        // term driven
        VALUE_ENCODERS.add(new ContextValueEncoder(new ContextDictionary()));
//        VALUE_ENCODERS.add(new IdValueEncoder());
        VALUE_ENCODERS.add(new TypeValueEncoder());
        
        // type driven
        VALUE_ENCODERS.add(new XsdDateTimeValueEncoder());
        VALUE_ENCODERS.add(new XsdDateValueEncoder());
        VALUE_ENCODERS.add(new MultibaseValueEncoder());
        VALUE_ENCODERS.add(new VocabValueEncoder());
        
        // value driven
        VALUE_ENCODERS.add(new UuidValueEncoder());
        VALUE_ENCODERS.add(new DidKeyValueEncoder());
    }

    public static final boolean COMPACT_ARRAYS = false;

    @Override
    public boolean isCompactArrays() {
        return COMPACT_ARRAYS;
    }

    @Override
    public Collection<ValueEncoder> getValueEncoders() {
        return VALUE_ENCODERS;
    }

    @Override
    public DictionaryAlgorithm getDictonaryAlgorithm() {
        return DictionaryAlgorithm.OnlyAppliedContextInProcessingOrder;
    }
}
