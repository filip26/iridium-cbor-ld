package com.apicatalog.cborld.decoder;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.CborLd.Version;
import com.apicatalog.cborld.decoder.value.ValueDecoder;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.registry.DocumentDictionary;

class DecoderConfigBuilder implements DecoderConfig {

    Version version;
    boolean compactArrays;
    Map<Integer, DocumentDictionary> registry;
    Collection<ValueDecoder> valueDecoders;
    DecoderMappingProvider decoderMapping;

    protected DecoderConfigBuilder() {
        // protected
    }

    static final DecoderConfigBuilder of(DecoderConfig config) {
        DecoderConfigBuilder builder = new DecoderConfigBuilder();
        builder.version = config.version();
        builder.compactArrays = config.isCompactArrays();
        if (config.version() != Version.V05) {
            builder.registry = new HashMap<Integer, DocumentDictionary>(config.registry());
        }
        builder.valueDecoders = config.valueDecoders();
        builder.decoderMapping = config.decoderMapping();
        return builder;
    }

    @Override
    public Version version() {
        return version;
    }

    @Override
    public boolean isCompactArrays() {
        return compactArrays;
    }

    @Override
    public Map<Integer, DocumentDictionary> registry() {
        return registry;
    }

    @Override
    public Collection<ValueDecoder> valueDecoders() {
        return valueDecoders;
    }

    @Override
    public DecoderMappingProvider decoderMapping() {
        return decoderMapping;
    }
}
