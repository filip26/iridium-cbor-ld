package com.apicatalog.cursor.jakarta;

import jakarta.json.JsonValue;

final class JakartaCursorState  {

    private final JsonValue data;
    private final Integer index;
    private final String key;

    JakartaCursorState(
            JsonValue data,
            Integer index,
            String key
            ) {
        this.data = data;
        this.index = index;
        this.key = key;
    }

    public JsonValue data() {
        return data;
    }
    
    public Integer index() {
        return index;
    }
    
    public String key() {
        return key;
    }
}
