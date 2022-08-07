package com.apicatalog.cursor;

public interface Cursor<T>  {

    boolean prev();

    MapCursor mapCursor();

    ArrayCursor arrayCursor();

    boolean isArrayItem();

    ArrayItemCursor arrayItemCursor();

    boolean isMapEntry();

    MapEntryCursor mapEntryCursor();

    MapEntryCursor mapKey(String key);

    MapEntryCursor entry(String key);

    T sourceValue();
    
    String key();
    Integer index();

    ArrayItemCursor arrayIndex(int index);

    ArrayItemCursor item(int index);
}
