package com.apicatalog.cborld.encoder;

import java.util.ArrayList;
import java.util.Collection;

import com.apicatalog.cborld.dictionary.ContextDictionary;
import com.apicatalog.cborld.encoder.value.ContextValueEncoder;
import com.apicatalog.cborld.encoder.value.IdValueEncoder;
import com.apicatalog.cborld.encoder.value.StringValueEncoder;
import com.apicatalog.cborld.encoder.value.TypeValueEncoder;
import com.apicatalog.cborld.encoder.value.UuidValueEncoder;
import com.apicatalog.cborld.encoder.value.ValueEncoder;
import com.apicatalog.cborld.encoder.value.XsdDateTimeValueEncoder;
import com.apicatalog.cborld.encoder.value.XsdDateValueEncoder;

public class DefaultEncoderConfig implements EncoderConfigration {

    public static final Collection<ValueEncoder> VALUE_ENCODERS = new ArrayList<>();

    static {
        VALUE_ENCODERS.add(new ContextValueEncoder(new ContextDictionary()));
        VALUE_ENCODERS.add(new IdValueEncoder());
        VALUE_ENCODERS.add(new TypeValueEncoder());
        VALUE_ENCODERS.add(new XsdDateTimeValueEncoder());
        VALUE_ENCODERS.add(new XsdDateValueEncoder());
        VALUE_ENCODERS.add(new UuidValueEncoder());
        VALUE_ENCODERS.add(new StringValueEncoder());
    }

    public static final boolean COMPACT_ARRAYS = true;

    @Override
    public boolean isCompactArrays() {
        return COMPACT_ARRAYS;
    }

    @Override
    public Collection<ValueEncoder> getValueEncoders() {
        return VALUE_ENCODERS;
    }
}
