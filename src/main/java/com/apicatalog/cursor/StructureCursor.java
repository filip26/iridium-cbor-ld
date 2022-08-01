package com.apicatalog.cursor;

public interface StructureCursor extends ValueCursor {

    int size();
    
    default boolean isEmpty() {
        return size() == 0;
    }
}
