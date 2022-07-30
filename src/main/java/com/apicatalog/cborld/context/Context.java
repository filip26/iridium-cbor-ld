package com.apicatalog.cborld.context;

import com.apicatalog.json.cursor.JsonObjectCursor;
import com.apicatalog.json.cursor.jakarta.JakartaJsonCursor;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.loader.DocumentLoader;

public class Context {

    public static TypeMapping getTypeMapping(JsonObjectCursor document, DocumentLoader loader) throws JsonLdError {
        
        final JsonLdOptions options = new JsonLdOptions();
        options.setOrdered(true);
        options.setDocumentLoader(loader);
        
        final ActiveContext activeContext = new ActiveContext(null, null, options);

        return Expansion
                    .with(activeContext, ((JakartaJsonCursor)document).getJsonObjecT(), null, null)
                    .typeMapping();
    }
    
}
