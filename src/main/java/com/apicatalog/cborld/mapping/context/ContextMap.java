package com.apicatalog.cborld.mapping.context;

import java.net.URI;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.function.Consumer;

import com.apicatalog.cborld.mapping.TypeKeyNameMapper;
import com.apicatalog.cborld.mapping.TypeMap;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.processor.ExecutionEvents;
import com.apicatalog.jsonld.processor.ExecutionEvents.TypeMapper;
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

    public static ContextMap newMap(Object node, TreeAdapter adapter, URI base, DocumentLoader loader) throws JsonLdException {

        final var options = Options.newOptions()
                .ordered(false)
                .loader(loader)
                .useInlineContexts(false)
                .base(base);

        final var appliedContextKeys = new LinkedHashSet<Collection<String>>();
        final var keyTypeMapper = new TypeMapperImpl();

        Expander.expand(
                new TreeIO(node, adapter),
                options,
                ExecutionEvents.of(options)
                        .contextKeyCollector(appliedContextKeys::add)
                        .keyTypeMapper(keyTypeMapper));

        return new ContextMap(keyTypeMapper.typeMap(), appliedContextKeys);
    }

    public static ContextMap from(
            Object node,
            TreeAdapter adapter,
            URI base,
            DocumentLoader loader,
            Consumer<Collection<String>> appliedContexts,
            TypeKeyNameMapper typeMapper) throws JsonLdException {

        final var options = Options.newOptions()
                .ordered(false)
                .loader(loader)
                .base(base);

        final var appliedContextKeys = new LinkedHashSet<Collection<String>>();
        final var keyTypeMapper = new TypeMapperImpl(typeMapper);

        Expander.expand(
                new TreeIO(node, adapter),
                options,
                ExecutionEvents.of(options)
                        .contextKeyCollector(appliedContexts.andThen(appliedContextKeys::add))
                        .keyTypeMapper(keyTypeMapper));

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
        public void onBeginMap(String key) {
            if (typeMapper != null) {
                typeMapper.beginMap(key);
            }
            var map = new LinkedHashMap<String, Object>();
            stack.peek().put(key, map);
            stack.push(map);
        }

        @Override
        public void onEndMap(String key) {
            if (typeMapper != null) {
                typeMapper.endMap();
            }
            stack.pop();
        }

        @Override
        public void onType(String key, String id) {
            if (typeMapper != null && Keywords.TYPE.equals(id)) {
                typeMapper.typeKeyName(key);
            }
            stack.peek().put(key, id);
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
        public String getType(String term) {
            if (typeMap.get(term) instanceof String string) {
                return string;
            }
            return null;
        }

        @Override
        public TypeMap getMapping(String term) {
            if (typeMap.get(term) instanceof Map map) {

                @SuppressWarnings("unchecked")
                final var typedMap = (Map<String, Object>) map;

                return new TypeMapImpl(typedMap);
            }
            return null;
        }
    }
}
