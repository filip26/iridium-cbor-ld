package com.apicatalog.cborld.context;

import java.net.URI;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import com.apicatalog.cborld.mapping.TypeKeyNameMapper;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.jsonld.JsonLd.Version;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.processor.Execution;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.jsonld.processor.TypeMapper;
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

        final var appliedContextKeys = new LinkedHashSet<Collection<String>>();
        final var keyTypeMapper = new TypeMapperImpl();

        final var runtime = Execution.of(options)
                .contextKeyCollector(appliedContextKeys::add)
                .keyTypeMapper(keyTypeMapper);

        Expander.expand(new TreeIO(node, adapter), options, runtime);

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
        final var keyTypeMapper = new TypeMapperImpl(typeMapper);

        final var runtime = Execution.of(options)
                .contextKeyCollector(appliedContexts.andThen(appliedContextKeys::add))
                .keyTypeMapper(keyTypeMapper);

        Expander.expand(new TreeIO(document, adapter),
                new Context.Builder(Version.V1_1).build(),
                base,
                options, runtime);

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

    private static class TypeMapperImpl implements TypeMapper {

        final Deque<Map<String, Object>> stack;
        TypeKeyNameMapper typeMapper;

        public TypeMapperImpl() {
            this.stack = new ArrayDeque<>();
            this.stack.push(new LinkedHashMap<String, Object>());
            this.typeMapper = null;
        }

        public TypeMapperImpl(TypeKeyNameMapper typeMapper) {
            this.stack = new ArrayDeque<>();
            this.stack.push(new LinkedHashMap<String, Object>());
            this.typeMapper = typeMapper;
        }

        @Override
        public void beginMap(String key) {
            if (typeMapper != null) {
                typeMapper.beginMap(key);
            }
            var map = new LinkedHashMap<String, Object>();
            stack.peek().put(key, map);
            stack.push(map);
        }

        @Override
        public void endMap() {
            if (typeMapper != null) {
                typeMapper.endMap();
            }
            stack.pop();
        }

        @Override
        public void mapType(String key, String id) {
            if (typeMapper != null) {
//                typeMapper.beginMap(key);
                typeMapper.typeKeyName(key);
//                typeMapper.endMap();
            }
            stack.peek().put(key, id);
        }

        @Override
        public void mapProperty(String key, String type, String value) {
        }

        public TypeMap typeMap() {
            return new TypeMapImpl(stack.peek());
        }
    }

    private static class TypeMapImpl implements TypeMap {

        final Map<String, Object> typeMap;

        TypeMapImpl(final Map<String, Object> typeMap) {
            this.typeMap = typeMap;
        }

        @Override
        public Collection<String> getType(String term) {

            var type = typeMap.get(term);
            if (type instanceof Map map) {
                type = map.get(Keywords.TYPE);
            }

            if (type instanceof String string) {
                return List.of(string);

            } else if (type instanceof Collection<?> array) {
                return array.stream().map(String.class::cast).toList();
            }

            return List.of();
        }

        @Override
        public TypeMap getMapping(String term) {

            var type = typeMap.get(term);
            if (type instanceof Map map) {
                return new TypeMapImpl((Map<String, Object>)map);
            }
            return null;
        }
    }
}
