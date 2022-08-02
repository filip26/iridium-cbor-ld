package com.apicatalog.cursor.cbor;

import java.util.Collections;
import java.util.Iterator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.ArrayItemCursor;
import com.apicatalog.cursor.Cursor;
import com.apicatalog.cursor.ValueCursor;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;

public class CborArrayCursor extends CborValueCursor implements ArrayCursor {
    
    public CborArrayCursor(final Cursor<DataItem> cursor) {
        super(cursor, cursor::sourceValue);
    }

    @Override
    public int size() {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return ((Array)value.get()).getDataItems().size();
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

    public Stream<ValueCursor> stream() {
        return StreamSupport.stream(this.spliterator(), false).map(ValueCursor.class::cast);
    }
}
