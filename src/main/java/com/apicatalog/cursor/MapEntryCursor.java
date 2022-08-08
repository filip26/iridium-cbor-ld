package com.apicatalog.cursor;

import java.math.BigInteger;

public interface MapEntryCursor extends DataCursor<MapCursor> {

    boolean hasNumericKey();
    boolean hasStringKey();

    String stringKey();
    BigInteger numericKey();

    MapEntryCursor key(String key);
    MapEntryCursor key(BigInteger key);
    
    @Override
    MapCursor parent();
}
