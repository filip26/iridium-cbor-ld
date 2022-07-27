package com.apicatalog.json.cursor;

public interface JsonCursor extends
                                JsonValueCursor,
                                JsonArrayCursor,
                                JsonObjectCursor,
                                Cloneable {
    JsonCursor clone();

    JsonCursor reset();
}
