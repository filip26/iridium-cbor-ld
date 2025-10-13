package com.apicatalog.cborld.mapping;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.tree.io.NodeAdapter;

@FunctionalInterface
public interface EncoderMappingProvider {

    Mapping getEncoderMapping(Object document, NodeAdapter adapter, Encoder encoder) throws ContextError;
}
