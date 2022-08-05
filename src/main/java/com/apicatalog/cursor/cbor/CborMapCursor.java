package com.apicatalog.cursor.cbor;

import java.math.BigInteger;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.apicatalog.cursor.Cursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.MapEntryCursor;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.UnsignedInteger;

public class CborMapCursor extends CborValueCursor implements MapCursor {

    final Function<DataItem, String> dataToKey;
    final Function<String, DataItem> keyToData;
    
    public CborMapCursor(Cursor<DataItem> cursor, Function<DataItem, String> dataToKey, Function<String, DataItem> keyToData) {
        super(cursor, cursor::sourceValue);
        this.dataToKey = dataToKey;
        this.keyToData = keyToData;
    }

    @Override
    public int size() {
        if (!isMap()) {
            throw new ClassCastException();
        }
        return ((Map)value.get()).getKeys().size();
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
    public Collection<String> keys() {
        if (!isMap()) {
            throw new ClassCastException();
        }

        return ((Map)value.get()).getKeys().stream().map(dataToKey).collect(Collectors.toList());
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

        DataItem item = keyToData.apply(key);

        if (((Map)value.get()).getKeys().contains(item)) {
            return true;
        }

        if (MajorType.UNSIGNED_INTEGER.equals(item.getMajorType())) {
            item = new UnsignedInteger(((UnsignedInteger)item).getValue().add(BigInteger.ONE));
            return (((Map)value.get()).getKeys().contains(item));            
        }
        
        return false;

    }
}
