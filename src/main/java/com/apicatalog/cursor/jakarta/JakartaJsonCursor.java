package com.apicatalog.cursor.jakarta;

import java.util.ArrayDeque;
import java.util.Deque;

import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.ArrayItemCursor;
import com.apicatalog.cursor.Cursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.MapEntryCursor;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

public class JakartaJsonCursor implements Cursor<JsonValue> {

    final Deque<JsonValue> path;
    final Deque<Object> indices;
    
    final ArrayItemCursor arrayItemCursor;
    final MapEntryCursor mapEntryCursor;
    
    final MapCursor mapCursor;
    final ArrayCursor arrayCursor;

    protected JakartaJsonCursor(JsonValue root) {
        
        this.path = new ArrayDeque<>();
        this.path.add(root);
        
        this.indices = new ArrayDeque<>();
        

        this.arrayItemCursor = new JakartaArrayItemCursor(this, this::sourceValue);
        this.mapEntryCursor = new JakartaMapEntryCursor(this, this::sourceValue);
        
        this.arrayCursor = new JakartaArrayCursor(this);
        this.mapCursor = new JakartaMapCursor(this);
    }
    
    public static ArrayCursor from(JsonArray array) {
        return new JakartaJsonCursor(array).arrayCursor();
    }

    public static MapCursor from(JsonObject map) {
        return new JakartaJsonCursor(map).mapCursor();
    }

    @Override
    public JsonValue sourceValue() {
        if (path.isEmpty()) {
            throw new IndexOutOfBoundsException();
        }
        return path.peek();
    }

    @Override
    public boolean prev() {
        if (path.size() == 1) {
            return false;
        }
        path.pop();
        indices.pop();
        return true;        
    }
    
    // horizontal
    @Override
    public MapEntryCursor entry(String mapKey) {
        indices.push(mapKey);;
        path.push(path.peek().asJsonObject().get(mapKey));
        return mapEntryCursor;
    }

    // horizontal
    @Override
    public ArrayItemCursor item(int arrayIndex) {
        indices.push(arrayIndex);
        path.push(path.peek().asJsonArray().get(arrayIndex));
        return arrayItemCursor;
    }

    // vertical
    @Override
    public MapEntryCursor mapKey(String mapKey) {
        indices.pop();
        indices.push(mapKey);

        path.pop();
        path.push(path.peek().asJsonObject().get(mapKey));

        return mapEntryCursor;
    }

    // vertical
    @Override
    public ArrayItemCursor arrayIndex(int arrayIndex) {
        indices.pop();
        indices.push(arrayIndex);

        path.pop();
        path.push(path.peek().asJsonArray().get(arrayIndex));

        return arrayItemCursor;
    }

    @Override
    public MapCursor mapCursor() {
        return mapCursor;
    }
    
    @Override
    public ArrayCursor arrayCursor() {
        return arrayCursor;
    }
    
    @Override
    public MapEntryCursor mapEntryCursor() {
        return mapEntryCursor;
    }
    
    @Override
    public ArrayItemCursor arrayItemCursor() {
        return arrayItemCursor;
    }
    
    @Override
    public Object index() {
        return indices.peek();
    }
    
    @Override
    public String toString() {
       return new StringBuilder()
           .append(JakartaJsonCursor.class.getSimpleName())
           .append('[')
           .append("depth=")
           .append(path.size())
           .append(", indices=")
           .append(indices)           
           .append(", path=")
           .append(path)
           .append(']')
           .toString();
    }

    @Override
    public boolean isArrayItem() {
        return !indices.isEmpty() && (indices.peek() instanceof Integer);
    }
    
    @Override    
    public boolean isMapEntry() {
        return !indices.isEmpty() && (indices.peek() instanceof String);
    }
}
