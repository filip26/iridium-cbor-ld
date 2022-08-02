package com.apicatalog.cursor.jakarta;

import java.util.function.Supplier;

import com.apicatalog.cursor.MapEntryCursor;

import jakarta.json.JsonValue;

final class JakartaMapEntryCursor extends JakartaValueCursor implements MapEntryCursor {

    public JakartaMapEntryCursor(final JakartaJsonCursor cursor, final Supplier<JsonValue> value) {
        super(cursor, value);
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
}
