package com.apicatalog.cursor.cbor;

import com.apicatalog.cursor.ArrayItemCursor;
import com.apicatalog.cursor.Cursor;

import co.nstant.in.cbor.model.DataItem;

public class CborArrayItemCursor extends CborValueCursor implements ArrayItemCursor {

    public CborArrayItemCursor(Cursor<DataItem> cursor) {
        super(cursor, cursor::sourceValue);
    }

    @Override
    public int arrayIndex() {
        if (!isArrayItem()) {
            throw new ClassCastException();    
        }        
        return cursor.index();
    }

    @Override
    public ArrayItemCursor arrayIndex(int index) {
        if (!isArrayItem()) {
            throw new ClassCastException();
        }
        
        return cursor.arrayIndex(index);
    }
    
    @Override
    public ArrayItemCursor asArrayItem() {
        return this;
    }
}
