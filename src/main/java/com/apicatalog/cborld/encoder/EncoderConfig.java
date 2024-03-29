package com.apicatalog.cborld.encoder;

import java.util.Collection;

import com.apicatalog.cborld.config.Config;
import com.apicatalog.cborld.encoder.value.ValueEncoder;

public interface EncoderConfig extends Config {

    Collection<ValueEncoder> valueEncoders();

}
