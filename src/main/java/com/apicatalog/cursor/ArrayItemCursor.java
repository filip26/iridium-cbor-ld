package com.apicatalog.cursor;

public interface ArrayItemCursor extends ValueCursor {

    int arrayIndex();
    
    ArrayItemCursor arrayIndex(int index);
    
}
