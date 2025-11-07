package com.apicatalog.cborld.context;

import java.net.URI;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.function.Consumer;

import com.apicatalog.cborld.mapping.TypeKeyNameMapper;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.processor.ProcessingRuntime;
import com.apicatalog.tree.io.TreeAdapter;

public class ContextMap {

    private final TypeMap typeMapping;
    private final Collection<Collection<String>> appliedContextKeys;

    protected ContextMap(TypeMap typeMapping, Collection<Collection<String>> appliedContextKeys) {
        this.typeMapping = typeMapping;
        this.appliedContextKeys = appliedContextKeys;
    }

    public static ContextMap from(Object document, TreeAdapter adapter, URI base, DocumentLoader loader) throws JsonLdError {

        final JsonLdOptions options = new JsonLdOptions();
        options.setOrdered(false);
        options.setDocumentLoader(loader);
        options.setBase(base);

        final ActiveContext activeContext = new ActiveContext(null, null, ProcessingRuntime.of(options));

        Collection<Collection<String>> appliedContextKeys = new LinkedHashSet<>();

        final TypeMap typeMapping = Expansion.with(
                activeContext,
                document,
                adapter,
                null,
                base,
                appliedContextKeys::add,
                null)
                .typeMapping();

        return new ContextMap(typeMapping, appliedContextKeys);

    }

    public static ContextMap from(
            Object document,
            TreeAdapter adapter,
            URI base,
            DocumentLoader loader,
            Consumer<Collection<String>> appliedContexts,
            TypeKeyNameMapper typeMapper) throws JsonLdError {

        final JsonLdOptions options = new JsonLdOptions();
        options.setOrdered(false);
        options.setDocumentLoader(loader);
        options.setBase(base);

        final ActiveContext activeContext = new ActiveContext(null, null, ProcessingRuntime.of(options));

        Collection<Collection<String>> appliedContextKeys = new LinkedHashSet<>();

        final TypeMap typeMapping = Expansion.with(
                activeContext,
                document,
                adapter,
                null,
                base,
                appliedContexts.andThen(appliedContextKeys::add),
                typeMapper)
                .typeMapping();

        return new ContextMap(typeMapping, appliedContextKeys);

    }

    public TypeMap getTypeMapping() {
        return typeMapping;
    }

    public Collection<Collection<String>> getContextKeySets() {
        return appliedContextKeys;
    }
}
