package com.apicatalog.cborld.dictionary;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
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

    int lastCustomIndex;

    protected CodeTermMap(Map<Integer, String> index, ActiveContext context, int lastCustomIndex) {
        this.index = index;
        this.reverse = index
                .entrySet()
                       .stream()
                       .collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey));
    
        this.context = context;
    
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
        
            return add(activeContext,
                new CodeTermMap(
                    new LinkedHashMap<>(KeywordDictionary.CODE_TO_TERM),
                    activeContext,
                    KeywordDictionary.CUSTOM_OFFSET
                ));
    
        } catch (JsonLdError e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            throw new ContextError(Code.InvalidContext, e);
        }
    }

    final static CodeTermMap add(final ActiveContext activeContext, final CodeTermMap map) throws JsonLdError {
    
        String[] sorted = activeContext.getTerms().stream().sorted().toArray(String[]::new);
    
        Arrays.stream(sorted).forEach(map::add);
    
        // scoped contexts
        for (final String key : sorted) {
    
            final TermDefinition def = activeContext.getTerm(key).orElseThrow(IllegalStateException::new);

            if (def.hasLocalContext()) {
    
                add(new ActiveContext(activeContext.getOptions())
                            .newContext()
                            .create(
                                def.getLocalContext(),
                                def.getBaseUrl()
                        ),
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
//    System.out.println(">>>> GET DEF " + term + " of " + parent + ", by type " + types  );
    context.getTerms().forEach(System.out::println);
        if (parent == null) {
            return getDefinition(term);
        }
    
        return null;
    }

    public ActiveContext getContext() {
        return context;
    }
    
//    private String processTypeScoped(final ActiveContext typeContext) throws JsonLdError {
//
//        String typeKey = null;
//
//        // 11.
//        for (final String key : Utils.index(element.keySet(), true)) {
//
//            final String expandedKey =
//                        activeContext
//                            .uriExpansion()
//                            .vocab(true)
//                            .expand(key);
//
//            if (!Keywords.TYPE.equals(expandedKey)) {
//                continue;
//
//            } else if (typeKey == null) {
//                typeKey = key;
//            }
//
//            // 11.2
//            final List<String> terms = JsonUtils
//                                            .toStream(element.get(key))
//                                            .filter(JsonUtils::isString)
//                                            .map(JsonString.class::cast)
//                                            .map(JsonString::getString)
//                                            .sorted()
//                                            .collect(Collectors.toList());
//
//            for (final String term : terms) {
//
//                Optional<JsonValue> localContext = typeContext.getTerm(term).map(TermDefinition::getLocalContext);
//
//                if (localContext.isPresent()) {
//
//                    Optional<TermDefinition> valueDefinition = activeContext.getTerm(term);
//
//                    activeContext =
//                            activeContext
//                                .newContext()
//                                .propagate(false)
//                                .create(localContext.get(),
//                                        valueDefinition
//                                                .map(TermDefinition::getBaseUrl)
//                                                .orElse(null)
//                                        );
//                }
//            }
//        }
//
//        return typeKey;
//    }

    
}
