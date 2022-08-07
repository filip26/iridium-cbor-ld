package com.apicatalog.cursor.jakarta;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import com.apicatalog.cursor.ArrayCursor;

import jakarta.json.Json;
import jakarta.json.JsonArray;

@DisplayName("Jakarta Cursor API Test")
@TestMethodOrder(OrderAnnotation.class)
class JakartaCursorTest {

    @Test
    void testRootArrayOfPrimitives() {
        
        JsonArray array = Json.createArrayBuilder()
                .add("1")
                .add(false)
                .add(1)
                .build();
        
        ArrayCursor cursor = JakartaJsonCursor.from(array);
        assertNotNull(cursor);
        assertTrue(cursor.isArray());
        assertEquals(3, cursor.size());
//
//        for (int i = 0; i < cursor.size(); i++) {
//            ValueCursor value = cursor.value(i);
//            
//            assertNotNull(value);
//            assertTrue(value.isPrimitive());
//            
//            assertTrue(cursor.isArray());
//            assertEquals(3, cursor.size());
//            assertTrue(cursor.isPrimitive(i));
//        }
    }

    @Test
    void testRootArrayOfArray() {
        
        JsonArray array = Json.createArrayBuilder()
                .add(Json.createArrayBuilder().add(1))
                .add(false)
                .build();
        
        ArrayCursor cursor = JakartaJsonCursor.from(array);
        
        assertNotNull(cursor);
        assertTrue(cursor.isArray());
        assertEquals(2, cursor.size());
        
//        ValueCursor value = cursor.value(0);
//        assertNotNull(value);
//        assertTrue(value.isArray());
//        assertTrue(cursor.isArray());
//        assertEquals(2, cursor.size());
//
//        ArrayCursor cursor2 = value.asArray();
//        assertNotNull(cursor2);
//        assertTrue(cursor2.isArray());
//        assertEquals(1, cursor2.size());
//        
//        assertNotNull(value);
//        assertTrue(value.isArray());
//
//        assertTrue(cursor.isArray());
//        assertEquals(1, cursor.size());
//
//        value = cursor.value(0);
//
//        assertNotNull(cursor2);
//        assertTrue(cursor2.isArray());
//        assertEquals(1, cursor2.size());
//        
//        assertNotNull(value);
//        assertTrue(value.isNumber());
//        
//        cursor2.parent();
//        
//        assertNotNull(cursor2);
//        assertTrue(cursor2.isArray());
//        assertEquals(2, cursor2.size());
        
    }

}
