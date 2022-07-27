package com.apicatalog.cborld.dictionary;

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

public class CodecTermMap {

    final Map<Integer, String> index;
    final Map<String, Integer> reverse;
    
    final ActiveContext context;
    final Map<Integer, ActiveContext> properyContexts;

    int lastCustomIndex;

    protected CodecTermMap(Map<Integer, String> index, ActiveContext context, Map<Integer, ActiveContext> properyContexts, int lastCustomIndex) {
	this.index = index;
	this.reverse = index
			.entrySet()
		       	.stream()
		       	.collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey));	

	this.context = context;
	this.properyContexts = properyContexts;
	
	this.lastCustomIndex = lastCustomIndex;
    }

    public static CodecTermMap from(Collection<String> contextUrls, DocumentLoader loader) throws ContextError {

	try {	
	    final JsonLdOptions options = new JsonLdOptions();
	    options.setDocumentLoader(loader);
	    
	    ActiveContext activeContext = new ActiveContext(options);
	    
	    JsonArrayBuilder bb = Json.createArrayBuilder();
	    contextUrls.forEach(bb::add);
//	    System.out.println(activeContext);
//	    System.out.println(" >>> " + bb.build() + ", " + bb.build());
	    activeContext = ActiveContextBuilder.with(activeContext).create(bb.build(), null);
	    
	    Map<Integer, ActiveContext> properyContexts = new HashMap<>();
	        
	    return add(activeContext, properyContexts,
			new CodecTermMap(
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

    final static CodecTermMap add(final ActiveContext activeContext, Map<Integer, ActiveContext> propertyContexts, final CodecTermMap map) throws JsonLdError {

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

    public String getTerm(int code) {
	return index.get(code);
    }
    
    public Integer getCode(String term) {
	return reverse.get(term);
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

    //FIXME remove
    public Collection<String> getTerms() {
	return context.getTerms();
    }
    
//    public boolean isType(String term) {
//	
//	
////	TermDefinition def = context.getTerm(term).orElseThrow(IllegalStateException::new);
////
////	ActiveContext propertyContext = properyContexts.get(def.hashCode());
////
////	if (propertyContext != null) {
////	    def = 
////	}
//	
//	return context.getTerm(term).map(t -> Keywords.TYPE.equals(t.getUriMapping())).orElse(false);
//    }
}
