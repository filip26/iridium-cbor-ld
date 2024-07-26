package com.apicatalog.cborld.config;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.cborld.CborLd;
import com.apicatalog.cborld.context.mapping.ContextMappingProvider;
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
import com.apicatalog.cborld.document.CompressedDocument;
import com.apicatalog.cborld.document.DocumentDictionary;
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
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;

public class V05Config extends BaseConfig implements EncoderConfig, DecoderConfig {

    public static final V05Config INSTANCE = new V05Config();

    static final ContextMappingProvider MAPPING = new ContextMappingProvider();

    static final Collection<ValueEncoder> VALUE_ENCODERS = new ArrayList<>();

    static {
        // term driven
        VALUE_ENCODERS.add(new ContextValueEncoder());

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
        VALUE_DECODERS.add(new ContextValueDecoder());

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

    public static final boolean STATIC_CONTEXTS = true;

    public static final byte VERSION = CborLd.VERSION_5_BYTE;

    static final Map<Integer, DocumentDictionary> DICTIONARIES;

    static {
        DICTIONARIES = new HashMap<>();
        DICTIONARIES.put(CompressedDocument.DICTIONARY.code(), CompressedDocument.DICTIONARY);
    }

    protected V05Config() {
        super(STATIC_CONTEXTS, COMPACT_ARRAYS);
    }

    @Override
    public Collection<ValueEncoder> valueEncoders() {
        return VALUE_ENCODERS;
    }

    @Override
    public Collection<ValueDecoder> valueDecoders() {
        return VALUE_DECODERS;
    }

    @Override
    public DecoderMappingProvider decoderMapping() {
        return MAPPING;
    }

    @Override
    public EncoderMappingProvider encoderMapping() {
        return MAPPING;
    }

    @Override
    public Map<Integer, DocumentDictionary> dictionaries() {
        return DICTIONARIES;
    }

    @Override
    public DocumentDictionary dictionary() {
        return CompressedDocument.DICTIONARY;
    }

    @Override
    public byte version() {
        return VERSION;
    }
}
