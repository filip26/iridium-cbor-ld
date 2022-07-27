package com.apicatalog.cborld.dictionary;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.context.ContextError.Code;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.ActiveContextBuilder;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.loader.DocumentLoader;

import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;

public class CodeTermMap implements Dictionary {

    final Map<Integer, String> index;
    final Map<String, Integer> reverse;

    final ActiveContext context;
    final Map<Integer, ActiveContext> properyContexts;

    int lastCustomIndex;

    protected CodeTermMap(Map<Integer, String> index, ActiveContext context, Map<Integer, ActiveContext> properyContexts, int lastCustomIndex) {
        this.index = index;
        this.reverse = index
                .entrySet()
                       .stream()
                       .collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey));
    
        this.context = context;
        this.properyContexts = properyContexts;
    
        this.lastCustomIndex = lastCustomIndex;
    }

    public static CodeTermMap from(Collection<String> contextUrls, DocumentLoader loader) throws ContextError {
    
        try {
            final JsonLdOptions options = new JsonLdOptions();
            options.setDocumentLoader(loader);
    
            ActiveContext activeContext = new ActiveContext(options);
    
            JsonArrayBuilder bb = Json.createArrayBuilder();
            contextUrls.forEach(bb::add);
    //        System.out.println(activeContext);
    //        System.out.println(" >>> " + bb.build() + ", " + bb.build());
            activeContext = ActiveContextBuilder.with(activeContext).create(bb.build(), null);
    
            Map<Integer, ActiveContext> properyContexts = new HashMap<>();
    
            return add(activeContext, properyContexts,
                new CodeTermMap(
                    new LinkedHashMap<>(KeywordDictionary.CODE_TO_TERM),
                    activeContext,
                    properyContexts,
                    KeywordDictionary.CUSTOM_OFFSET
                ));
    
        } catch (JsonLdError e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            throw new ContextError(Code.InvalidContext, e);
        }
    }

    final static CodeTermMap add(final ActiveContext activeContext, Map<Integer, ActiveContext> propertyContexts, final CodeTermMap map) throws JsonLdError {
    
        String[] sorted = activeContext.getTerms().stream().sorted().toArray(String[]::new);
    
        Arrays.stream(sorted).forEach(map::add);
    
        // scoped contexts
        for (final String key : sorted) {
    
            final TermDefinition def = activeContext.getTerm(key).orElseThrow(IllegalStateException::new);
    
            if (def.hasLocalContext()) {
    
            propertyContexts.put(def.hashCode(),
                //new ActiveContext(activeContext.getOptions())
                activeContext
                .newContext()
                .create(
                    def.getLocalContext(),
                                def.getBaseUrl()
                    ));
    
    
            add(
            new ActiveContext(activeContext.getOptions())
                .newContext()
                .create(
                    def.getLocalContext(),
                                def.getBaseUrl()
                    ),
                propertyContexts,
                map
                );
            }
        }
        return map;
    }

    void add(String key) {
        if (!reverse.containsKey(key)) {
                index.put(lastCustomIndex, key);
                reverse.put(key,  lastCustomIndex);
                lastCustomIndex += 2;
        }
    }

    @Override
    public String getValue(BigInteger code) {
        return index.get(code.intValueExact());
    }

    @Override
    public BigInteger getCode(String term) {
        return reverse.containsKey(term) ? BigInteger.valueOf(reverse.get(term)) : null;
    }

    public TermDefinition getDefinition(String term) {
        return context.getTerm(term).orElse(null);
    }

    public TermDefinition getDefinition(TermDefinition parent, String term) {
    
        if (parent == null) {
            return getDefinition(term);
        }
    
        ActiveContext activeContext = properyContexts.get(parent.hashCode());
        if (activeContext != null) {
            return activeContext.getTerm(term).orElse(null);
        }
        return null;
    }
}
