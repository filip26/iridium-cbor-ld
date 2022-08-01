package com.apicatalog.cursor.jakarta;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.MapEntryCursor;

import jakarta.json.JsonObject;

public class JakartaMapCursor extends JakartaValueCursor implements MapCursor {

    public JakartaMapCursor(final JakartaJsonCursor cursor) {
        super(cursor, cursor::jsonValue);
    }

    @Override
    public int size() {
        if (!isMap()) {
            throw new ClassCastException();
        }
        return cursor.jsonValue().asJsonObject().size();
    }

    @Override
    public Collection<String> keys() {
        if (!isMap()) {
            throw new ClassCastException();
        }
        return value.get().asJsonObject().keySet();
    }

    public JsonObject getJsonObject() {
        return (JsonObject) cursor.jsonValue();
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

        return new Iterator<>() {

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


//    @Override
//    public ArrayCursor array(final String key) {
//        if (!isArray(key)) {
//            throw new IllegalArgumentException();
//        }
//    
//        return cursor.next(cursor.structure().asJsonObject().get(key).asJsonArray());
//    }
//
//    @Override
//    public MapCursor object(final String key) {
//        if (!isObject(key)) {
//            throw new IllegalArgumentException();
//        }
//    
//        return cursor.next(cursor.structure().asJsonObject().get(key).asJsonObject());
//    }
//
//    @Override
//    public ValueCursor value(final String key) {
//        if (!isObject()) {
//            throw new IllegalArgumentException();
//        }
//    
//        return cursor.value(cursor.structure().asJsonObject().get(key));
//    }
    
//    @Override
//    public MapCursor clone() {
//        return cursor.clone().mapCursor();
//    }
    
}
