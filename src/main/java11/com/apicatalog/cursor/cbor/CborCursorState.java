package com.apicatalog.cursor.cbor;

import co.nstant.in.cbor.model.DataItem;

final class CborCursorState {

    private final DataItem data;
    private final Integer index;
    private final String key;
        
    CborCursorState(
            DataItem data,
            Integer index,
            String key
            ) {
        this.data = data;
        this.index = index;
        this.key = key;
    }

    public DataItem data() {
        return data;
    }
    
    public Integer index() {
        return index;
    }
    
    public String key() {
        return key;
    }    
}
