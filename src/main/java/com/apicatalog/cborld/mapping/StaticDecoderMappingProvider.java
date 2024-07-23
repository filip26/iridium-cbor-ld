package com.apicatalog.cborld.mapping;

import java.net.URI;
import java.util.Collection;
import java.util.List;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.model.DataItem;

public class StaticDecoderMappingProvider implements DecoderMappingProvider {

    protected final Dictionary dictionary;

    public StaticDecoderMappingProvider(Dictionary dictionary) {
        this.dictionary = dictionary;
    }

    @Override
    public Mapping getDecoderMapping(DataItem document, URI base, DocumentLoader loader, DecoderConfig config) throws DecoderError, ContextError {
        return new Mapping() {

            @Override
            public TypeMap typeMap() {
                return new TypeMap() {

                    @Override
                    public Collection<String> getType(String term) {
                        System.out.println("1 >> TERM " + term);
                        return List.of("ecdsa-rdfc-2019");
                    }

                    @Override
                    public TypeMap getMapping(String term) {
                        System.out.println("2 >> TERM " + term);
                        return this;
                    }
                };
            }

            @Override
            public Dictionary dictionary() {
                return dictionary;
            }
        };
    }

}
