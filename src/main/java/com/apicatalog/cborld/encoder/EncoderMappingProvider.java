package com.apicatalog.cborld.encoder;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.tree.io.TreeAdapter;

@FunctionalInterface
public interface EncoderMappingProvider {

    Mapping getEncoderMapping(Object document, TreeAdapter adapter, Encoder encoder) throws EncoderException;
}
