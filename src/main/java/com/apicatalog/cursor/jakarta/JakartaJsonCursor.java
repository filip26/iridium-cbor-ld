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

    record StackState(
            JsonValue data,
            Integer index,
            String key
            ) {};
    
    final Deque<StackState> stack;
    
    final ArrayItemCursor arrayItemCursor;
    final MapEntryCursor mapEntryCursor;
    
    final MapCursor mapCursor;
    final ArrayCursor arrayCursor;

    protected JakartaJsonCursor(JsonValue root) {
        
        this.stack = new ArrayDeque<>();
        this.stack.add(new StackState(root, null, null));
        
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
        if (stack.isEmpty()) {
            throw new IndexOutOfBoundsException();
        }
        return stack.peek().data;
    }

    @Override
    public boolean prev() {
        if (stack.size() == 1) {
            return false;
        }
        stack.pop();
        return true;        
    }
    
    // horizontal
    @Override
    public MapEntryCursor entry(String mapKey) {
        stack.push(new StackState(stack.peek().data.asJsonObject().get(mapKey), null, mapKey));
        return mapEntryCursor;
    }

    // horizontal
    @Override
    public ArrayItemCursor item(int arrayIndex) {
        stack.push(new StackState(stack.peek().data.asJsonArray().get(arrayIndex), arrayIndex, null));
        return arrayItemCursor;
    }

    // vertical
    @Override
    public MapEntryCursor mapKey(String mapKey) {
        stack.pop();
        return entry(mapKey);
    }

    // vertical
    @Override
    public ArrayItemCursor arrayIndex(int arrayIndex) {
        stack.pop();
        return item(arrayIndex);
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
    public Integer index() {
        return stack.peek().index;
    }
    
    @Override
    public String key() {
        return stack.peek().key;
    }
    
    @Override
    public String toString() {
       return new StringBuilder()
           .append(JakartaJsonCursor.class.getSimpleName())
           .append('[')
           .append("depth=")
           .append(stack.size())
           .append(", path=")
           .append(stack)
           .append(']')
           .toString();
    }

    @Override
    public boolean isArrayItem() {
        return !stack.isEmpty() && (stack.peek().index != null);
    }
    
    @Override    
    public boolean isMapEntry() {
        return !stack.isEmpty() && (stack.peek().key != null);
    }
}
