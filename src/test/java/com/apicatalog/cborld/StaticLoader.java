package com.apicatalog.cborld;

import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.jsonld.loader.DocumentLoader;

public class StaticLoader extends StaticContextLoader {

    static {
        staticCache.put("https://w3id.org/utopia/v2", get(StaticLoader.class, "utopia-v2.jsonld"));
    }
    
    public StaticLoader(DocumentLoader defaultLoader) {
        super(defaultLoader);
    }
}
