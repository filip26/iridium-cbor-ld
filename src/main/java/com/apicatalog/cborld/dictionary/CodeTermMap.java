package com.apicatalog.cborld.dictionary;

import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.context.ContextError.Code;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.ActiveContextBuilder;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

public class CodeTermMap implements Dictionary {

    final Map<Integer, String> index;
    final Map<String, Integer> reverse;

    int lastCustomIndex;

    protected CodeTermMap(Map<Integer, String> index, int lastCustomIndex) {
        this.index = index;
        this.reverse = index
                .entrySet()
                       .stream()
                       .collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey));
    
        this.lastCustomIndex = lastCustomIndex;
    }

    public static CodeTermMap from(Collection<String> contextUrls, Collection<String> scoped, DocumentLoader loader) throws ContextError {
    
        try {
            
            CodeTermMap map = new CodeTermMap(
                                      new LinkedHashMap<>(KeywordDictionary.CODE_TO_TERM),
                                      KeywordDictionary.CUSTOM_OFFSET
                                      );

            Collection<JsonObject> typeScoped = new ArrayList<>();
            
            for (String context : contextUrls) {
                
                Document document = loader.loadDocument(URI.create(context), new DocumentLoaderOptions());
                
                if (document == null || document.getJsonContent().isEmpty()) {
                    //TODO print warning
                    continue;
                }
                
                JsonStructure value = document.getJsonContent().get();
                
                from(value, map, loader);
                
                for (String typeContext : scoped) {
                    if (value.asJsonObject().getJsonObject("@context").containsKey(typeContext)) {
                        typeScoped.add(value.asJsonObject().getJsonObject("@context").getJsonObject(typeContext));
                    }
                }
                
            }
            
            for (JsonObject s : typeScoped) {
                from(s, map, loader);
            }

            return map;
//            final JsonLdOptions options = new JsonLdOptions();
//            options.setDocumentLoader(loader);
//    
//            ActiveContext activeContext = new ActiveContext(options);
//    
//            JsonArrayBuilder bb = Json.createArrayBuilder();
//            contextUrls.forEach(bb::add);
//    //        System.out.println(activeContext);
//    //        System.out.println(" >>> " + bb.build() + ", " + bb.build());
//            activeContext = ActiveContextBuilder.with(activeContext).create(bb.build(), null);
//        
//            return add(activeContext,
//                new CodeTermMap(
//                    new LinkedHashMap<>(KeywordDictionary.CODE_TO_TERM),
//                    KeywordDictionary.CUSTOM_OFFSET
//                ));
    
        } catch (JsonLdError e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            throw new ContextError(Code.InvalidContext, e);
        }
    }
    
    static void from(JsonObject object, CodeTermMap map, DocumentLoader loader) throws JsonLdError {
        
        String[] sorted = object.keySet()
                
                .stream()
                .sorted()
                .toArray(String[]::new)
                ;
//        if (map.size() > 100) {
//            sorted = object.keySet()
//                    
//                    .stream()
//
//                    .toArray(String[]::new)
//                    ;    
//        }
        System.out.println(">>> " + ( map.size() ) + ", " + Arrays.toString(sorted));
//        System.out.println(">>> " + ( map.size() ) + ", " + object.keySet());
        
        Arrays.stream(sorted).forEach(map::add);
        
        
        
        
        for (String key : object.keySet()) {
            
            if (Keywords.CONTEXT.equals(key)) {
                from(object.get(Keywords.CONTEXT), map, loader);
            }
//            System.out.println("# " + key);
//            
//        
//            
//            JsonValue value = object.get(key);
            
//            if (JsonUtils.isNotNull(value)) {
//                from(value, map, loader);
//            }
        }
    }

    static void from(JsonArray array, CodeTermMap map, DocumentLoader loader) throws JsonLdError {
        for (JsonValue value : array) {
            from(value, map, loader);
        }
    }

    static void from(JsonValue value, CodeTermMap map, DocumentLoader loader) throws JsonLdError {
        if (JsonUtils.isArray(value)) {
            from(value.asJsonArray(), map, loader);
            
        } else if (JsonUtils.isObject(value)) {
            from(value.asJsonObject(), map, loader);
        }
    }

    
    final static CodeTermMap add(final ActiveContext activeContext, final CodeTermMap map) throws JsonLdError {
    
        String[] sorted = activeContext.getTerms().stream().sorted().toArray(String[]::new);
        System.out.println(">>> " + ( map.size() ) + ", " + Arrays.toString(sorted));    
        Arrays.stream(sorted).forEach(map::add);
    
        // scoped contexts
        for (final String key 
              //  : sorted ) {
            : activeContext.getTerms()) {
        

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
    
    //FIXME remove
    public int size() {
        return lastCustomIndex;
    }
}
