package com.apicatalog.cborld.decoder;

import java.util.Collection;

import com.apicatalog.cborld.config.Config;
import com.apicatalog.cborld.decoder.value.ValueDecoder;

public interface DecoderConfig  extends Config {

    Collection<ValueDecoder> valueDecoders();
}
