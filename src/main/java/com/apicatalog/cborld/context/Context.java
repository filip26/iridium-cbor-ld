package com.apicatalog.cborld.context;

import java.net.URI;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.function.Consumer;

import com.apicatalog.cursor.MapCursor;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.loader.DocumentLoader;

public class Context {

    private final TypeMapping typeMapping;
    private final Collection<Collection<String>> appliedContextKeys;
    
    protected Context(TypeMapping typeMapping, Collection<Collection<String>> appliedContextKeys) {
        this.typeMapping = typeMapping;
        this.appliedContextKeys = appliedContextKeys;
    }
    
    public static Context from(MapCursor document, URI base, DocumentLoader loader) throws JsonLdError {

        final JsonLdOptions options = new JsonLdOptions();
        options.setOrdered(false);
        options.setDocumentLoader(loader);
        options.setBase(base);
        
        final ActiveContext activeContext = new ActiveContext(null, null, options);

        Collection<Collection<String>> appliedContextKeys = new LinkedHashSet<>();

        final TypeMapping typeMapping = Expansion.with(
                                            activeContext,
                                            document, 
                                            null, base, 
                                            appliedContextKeys::add,
                                            null
                                            )
                                        .typeMapping();

        return new Context(typeMapping, appliedContextKeys);

    }

    public static Context from(MapCursor document, URI base,  DocumentLoader loader, Consumer<Collection<String>> appliedContexts, TypeKeyNameMapper typeMapper) throws JsonLdError {

        final JsonLdOptions options = new JsonLdOptions();
        options.setOrdered(false);
        options.setDocumentLoader(loader);
        options.setBase(base);

        final ActiveContext activeContext = new ActiveContext(null, null, options);

        Collection<Collection<String>> appliedContextKeys = new LinkedHashSet<>();

        final TypeMapping typeMapping = Expansion.with(
                                            activeContext,
                                            document, 
                                            null, base, 
                                            appliedContexts.andThen(appliedContextKeys::add),
                                            typeMapper
                                            )
                                        .typeMapping();

        return new Context(typeMapping, appliedContextKeys);

    }

    public TypeMapping getTypeMapping() {
        return typeMapping;
    }
    
    public Collection<Collection<String>> getContextKeySets() {
        return appliedContextKeys;
    }
}
