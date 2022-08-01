package com.apicatalog.cursor;

import java.util.Collection;

public interface MapEntryCursor extends ValueCursor {

    String mapKey();
    
    MapEntryCursor mapKey(String key);

    Collection<String> keys();
    
}
