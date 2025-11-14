package com.apicatalog.cborld.context;

import java.net.URI;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.function.Consumer;

import com.apicatalog.cborld.mapping.TypeKeyNameMapper;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.processor.Execution;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIO;

public class ContextMap {

    private final TypeMap typeMapping;
    private final Collection<Collection<String>> appliedContextKeys;

    protected ContextMap(TypeMap typeMapping, Collection<Collection<String>> appliedContextKeys) {
        this.typeMapping = typeMapping;
        this.appliedContextKeys = appliedContextKeys;
    }

    public static ContextMap from(Object node, TreeAdapter adapter, URI base, DocumentLoader loader) throws JsonLdException {

        final var options = Options.newOptions()
                .ordered(false)
                .loader(loader)
                .base(base);

        final var keyTypeMapper = new KeyTypeMapperImpl();

        final var appliedContextKeys = new LinkedHashSet<Collection<String>>();
        
        final var runtime = Execution.of(options)
                .contextKeyCollector(appliedContextKeys::add)
                .keyTypeMapper(keyTypeMapper)
                ;
        
        Expander.expand(new TreeIO(node, adapter), options, runtime);

//        
//        final ActiveContext activeContext = new ActiveContext(null, null, ProcessingRuntime.of(options));
//
//        final TypeMap typeMapping = Expansion.with(
//                activeContext,
//                node,
//                adapter,
//                null,
//                base,
//                appliedContextKeys::add,
//                null)
//                .typeMapping();
//        try {
//            System.out.println("> " + new JakartaMaterializer().node(appliedContextKeys, NativeAdapter.instance()));
//        } catch (TreeIOException e) {
//            // TODO Auto-generated catch block
//            e.printStackTrace();
//        }
        return new ContextMap(keyTypeMapper.typeMap(), appliedContextKeys);

    }

    public static ContextMap from(
            Object document,
            TreeAdapter adapter,
            URI base,
            DocumentLoader loader,
            Consumer<Collection<String>> appliedContexts,
            TypeKeyNameMapper typeMapper) throws JsonLdException {

        final var options = Options.newOptions()
                .ordered(false)
                .loader(loader)
                .base(base);

//        final ActiveContext activeContext = new ActiveContext(null, null, ProcessingRuntime.of(options));

        final var appliedContextKeys = new LinkedHashSet<Collection<String>>();
        final var keyTypeMapper = new KeyTypeMapperImpl();
        
        
        final var runtime = Execution.of(options)
                .contextKeyCollector(appliedContexts.andThen(appliedContextKeys::add))
                .keyTypeMapper(keyTypeMapper)
                ;
        
        Expander.expand(new TreeIO(document, adapter), options, runtime);
        
//        final TypeMap typeMapping = Expansion.with(
//                activeContext,
//                document,
//                adapter,
//                null,
//                base,
//                appliedContexts.andThen(appliedContextKeys::add),
//                typeMapper)
//                .typeMapping();

        return new ContextMap(keyTypeMapper.typeMap(), appliedContextKeys);

    }

    public TypeMap getTypeMapping() {
        return typeMapping;
    }

    public Collection<Collection<String>> getContextKeySets() {
        return appliedContextKeys;
    }
}
