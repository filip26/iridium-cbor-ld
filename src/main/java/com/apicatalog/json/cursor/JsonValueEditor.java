package com.apicatalog.json.cursor;

import java.math.BigInteger;

public interface JsonValueEditor extends JsonValueCursor {

    JsonValueEditor set(String value);

    JsonValueEditor set( boolean value);

    JsonValueEditor set(long value);
    
    JsonValueEditor setNull();

    JsonValueEditor set(BigInteger value);

    JsonObjectEditor newObject();
    
    JsonArrayEditor newArray();
    
    JsonValueEditor done();
}
