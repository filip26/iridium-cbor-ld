package com.apicatalog.cursor;

public interface StructureCursor<P> extends DataCursor<P> {

    int size();
    
    default boolean isEmpty() {
        return size() == 0;
    }
}
