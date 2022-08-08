package com.apicatalog.cursor;

public interface ArrayItemCursor extends DataCursor {

    int arrayIndex();
    
    ArrayItemCursor arrayIndex(int index);
    
}
