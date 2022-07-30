package com.apicatalog.cborld.context;

import java.util.Collection;
import java.util.LinkedHashSet;

import com.apicatalog.json.cursor.JsonObjectCursor;
import com.apicatalog.json.cursor.jakarta.JakartaJsonCursor;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.loader.DocumentLoader;

public class Context {

    private final TypeMapping typeMapping;
    private final Collection<String> appliedTypeScopedContexts;
    
    protected Context(TypeMapping typeMapping, Collection<String> appliedTypeScopedContexts) {
        this.typeMapping = typeMapping;
        this.appliedTypeScopedContexts = appliedTypeScopedContexts;
    }
    
    public static Context from(JsonObjectCursor document, DocumentLoader loader) throws JsonLdError {

        final JsonLdOptions options = new JsonLdOptions();
        options.setOrdered(true);
        options.setDocumentLoader(loader);
        
        final ActiveContext activeContext = new ActiveContext(null, null, options);

        Collection<String> appliedTypeScopedContexts = new LinkedHashSet<>();
        
        final TypeMapping typeMapping = Expansion
                    .with(activeContext, ((JakartaJsonCursor)document).getJsonObjecT(), null, null, appliedTypeScopedContexts)
                    .typeMapping();
        
        return new Context(typeMapping, appliedTypeScopedContexts);

    }
    
    public TypeMapping getTypeMapping() {
        return typeMapping;
    }
    
    public Collection<String> getAppliedTypeScopedContexts() {
        return appliedTypeScopedContexts;
    }
}
