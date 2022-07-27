package com.apicatalog.cborld.encoder;

import java.util.Collection;

import com.apicatalog.cborld.encoder.value.ValueEncoder;

public interface EncoderConfigration {

    boolean isCompactArrays();

    Collection<ValueEncoder> getValueEncoders();
}
