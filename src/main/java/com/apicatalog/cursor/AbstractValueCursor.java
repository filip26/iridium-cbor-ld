package com.apicatalog.cursor;

import java.util.Optional;
import java.util.function.Supplier;

public abstract class AbstractValueCursor<T> implements ValueCursor {

    protected final Cursor<T> cursor;
    protected final Supplier<T> value;

    public AbstractValueCursor(final Cursor<T> cursor, Supplier<T> value) {
        this.cursor = cursor;
        this.value = value;
    }

    @Override
    public Optional<ValueCursor> parent() {
        return cursor.prev() 
                ? Optional.of(this)
                : Optional.empty();
    }

    @Override
    public MapCursor asMap() {
        if (!isMap()) {
            throw new ClassCastException();
        }
        return cursor.mapCursor();
    }

    @Override
    public ArrayCursor asArray() {
        if (!isArray()) {
            throw new ClassCastException();
        }
        return cursor.arrayCursor();
    }

    @Override
    public String toString() {
       return new StringBuilder()
           .append(getClass().getSimpleName())
           .append('[')
           .append("cursor=")
           .append(cursor.toString())
           .append(']')
           .toString();
    }

    @Override
    public boolean isArrayItem() {
        return cursor.isArrayItem();
    }

    @Override
    public ArrayItemCursor asArrayItem() {
        return cursor.arrayItemCursor();
    }

    @Override
    public boolean isMapEntry() {
        return cursor.isMapEntry();
    }

    @Override
    public MapEntryCursor asMapEntry() {
        return cursor.mapEntryCursor();
    }
}
