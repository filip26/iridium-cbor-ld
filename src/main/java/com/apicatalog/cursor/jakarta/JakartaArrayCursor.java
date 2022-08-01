package com.apicatalog.cursor.jakarta;

import java.util.Collections;
import java.util.Iterator;

import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.ArrayItemCursor;

public class JakartaArrayCursor extends JakartaValueCursor implements ArrayCursor {
    
    public JakartaArrayCursor(final JakartaJsonCursor cursor) {
        super(cursor, cursor::jsonValue);
    }

    @Override
    public int size() {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return cursor.jsonValue().asJsonArray().size();
    }

    @Override
    public ArrayItemCursor item(int index) {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return cursor.item(index);
    }

    @Override
    public Iterator<ArrayItemCursor> iterator() {
        if (!isArray()) {
            throw new ClassCastException();
        }
        
        if (isEmpty()) {
            return Collections.emptyIterator();
        }
        
        final int size = size();

        return new Iterator<>() {
            
            final ArrayItemCursor item = cursor.item(0);
                        
            int index = 0;

            @Override
            public boolean hasNext() {
                return index < size;
            }

            @Override
            public ArrayItemCursor next() {
                return item.arrayIndex(index++);
            }
        };
    }
    
    @Override
    public ArrayCursor asArray() {
        return this;
    }

//    @Override
//    public ArrayCursor array(int i) {
//        if (!isArray(i)) {
//            throw new IllegalArgumentException();
//        }    
//        return cursor.next(cursor.structure().asJsonArray().get(i).asJsonArray());
//    }
//
//    @Override
//    public MapCursor object(int i) {
//        if (!isObject(i)) {
//            throw new IllegalArgumentException();
//        }
//        return cursor.next(cursor.structure().asJsonArray().get(i).asJsonObject());
//    }
//
//    @Override
//    public ValueCursor value(int i) {
//        if (!isArray()) {
//            throw new ClassCastException();
//        }    
//        return cursor.value(cursor.structure().asJsonArray().get(i));
//    }

}
