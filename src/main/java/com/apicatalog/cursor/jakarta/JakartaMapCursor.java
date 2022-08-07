package com.apicatalog.cursor.jakarta;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.MapEntryCursor;

public class JakartaMapCursor extends JakartaValueCursor implements MapCursor {

    public JakartaMapCursor(final JakartaJsonCursor cursor) {
        super(cursor, cursor::sourceValue);
    }

    @Override
    public int size() {
        if (!isMap()) {
            throw new ClassCastException();
        }
        return cursor.sourceValue().asJsonObject().size();
    }

    @Override
    public Collection<String> keys() {
        if (!isMap()) {
            throw new ClassCastException();
        }
        return value.get().asJsonObject().keySet();
    }

    @Override
    public Iterator<MapEntryCursor> iterator() {
        if (!isMap()) {
            throw new ClassCastException();
        }

        if (isEmpty()) {
            return Collections.emptyIterator();            
        }

        final Iterator<String> iterator = keys().iterator();

        return new Iterator<MapEntryCursor>() {

            final MapEntryCursor entry = entry();
            
            @Override
            public boolean hasNext() {
                return iterator.hasNext();
            }

            @Override
            public MapEntryCursor next() {
                return  entry.mapKey(iterator.next());
            }
        };
    }

    @Override
    public MapEntryCursor entry(String key) {
        if (!isMap()) {
            throw new ClassCastException();
        }

        return cursor.entry(key);
    }

    @Override
    public boolean contains(String key) {
        if (!isMap()) {
            throw new ClassCastException();
        }

        return value.get().asJsonObject().containsKey(key);
    }
}
