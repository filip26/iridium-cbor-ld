package com.apicatalog.cursor.jakarta;

import java.util.Collections;
import java.util.Iterator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.ArrayItemCursor;
import com.apicatalog.cursor.ValueCursor;

public class JakartaArrayCursor extends JakartaValueCursor implements ArrayCursor {
    
    public JakartaArrayCursor(final JakartaJsonCursor cursor) {
        super(cursor, cursor::sourceValue);
    }

    @Override
    public int size() {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return cursor.sourceValue().asJsonArray().size();
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

        return new Iterator<ArrayItemCursor>() {
            
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
    
    public Stream<ValueCursor> stream() {
        return StreamSupport.stream(this.spliterator(), false).map(ValueCursor.class::cast);
    }
}
