package com.apicatalog.cursor;

public interface MapEntryCursor extends ValueCursor {

    String mapKey();

    MapEntryCursor mapKey(String key);
}
