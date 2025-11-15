package com.apicatalog.cborld.mapping;

import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.tree.io.TreeAdapter;

@FunctionalInterface
public interface EncoderMappingProvider {

    Mapping getEncoderMapping(Object document, TreeAdapter adapter, Encoder encoder) throws EncoderException;
}
