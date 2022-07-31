package com.apicatalog.cborld.dictionary;

import java.math.BigInteger;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.jsonld.loader.DocumentLoader;

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

    public static CodeTermMap from(Collection<String> contextUrls, Collection<Collection<String>> contextKeys, DocumentLoader loader) throws ContextError {
    
        final CodeTermMap map = new CodeTermMap(
                                  new LinkedHashMap<>(KeywordDictionary.CODE_TO_TERM),
                                  KeywordDictionary.CUSTOM_OFFSET
                                  );
        contextKeys
            .stream()
            .map(c -> c.stream().sorted())
            .flatMap(Function.identity())
            .forEach(map::add);
            
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
}
