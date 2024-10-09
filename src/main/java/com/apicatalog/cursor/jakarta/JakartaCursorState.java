package com.apicatalog.cursor.jakarta;

import jakarta.json.JsonValue;

record JakartaCursorState(
        JsonValue data,
        Integer index,
        String key
        ) {
}
