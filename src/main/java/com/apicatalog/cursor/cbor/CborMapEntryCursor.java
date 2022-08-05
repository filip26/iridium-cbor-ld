package com.apicatalog.cursor.cbor;

import com.apicatalog.cursor.Cursor;
import com.apicatalog.cursor.MapEntryCursor;

import co.nstant.in.cbor.model.DataItem;

final class CborMapEntryCursor extends CborValueCursor implements MapEntryCursor {

    public CborMapEntryCursor(final Cursor<DataItem> cursor) {
        super(cursor, cursor::sourceValue);
    }

    @Override
    public String mapKey() {
        if (!isMapEntry()) {
            throw new ClassCastException();    
        }        
        return (String)cursor.index();
    }

    @Override
    public MapEntryCursor mapKey(final String key) {
        if (!isMapEntry()) {
            throw new ClassCastException();
        }
        return cursor.mapKey(key);
    }

    public DataItem getValue() {
        return value.get();
    }
}
