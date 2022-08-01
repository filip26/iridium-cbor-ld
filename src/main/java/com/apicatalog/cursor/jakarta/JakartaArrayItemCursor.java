package com.apicatalog.cursor.jakarta;

import java.util.function.Supplier;

import com.apicatalog.cursor.ArrayItemCursor;

import jakarta.json.JsonValue;

public class JakartaArrayItemCursor extends JakartaValueCursor implements ArrayItemCursor {

    public JakartaArrayItemCursor(JakartaJsonCursor cursor, Supplier<JsonValue> value) {
        super(cursor, value);
    }

    @Override
    public int arrayIndex() {
        if (!isArrayItem()) {
            throw new ClassCastException();    
        }        
        return (Integer)cursor.index();
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
