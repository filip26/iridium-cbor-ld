package com.apicatalog.cursor.jakarta;

import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.Cursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.ValueCursor;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

public class JakartaJsonCursor implements Cursor {

    //TODO use dynamic lazy index
    final JsonValue[] path;    
    int index;
    
    MapCursor.Key[] indices;
    
//    final

//    final int[] marks;
    
//    Function<JakartaJsonCursor, ValueCursor> valueCursor;
//    Function<JakartaJsonCursor, MapCursor> mapCursor;
//    Function<JakartaJsonCursor, ArrayCursor> arrayCursor;

    final ValueCursor valueCursor;  //TODO always + 1
    final MapCursor mapCursor;
    final ArrayCursor arrayCursor;

    public JakartaJsonCursor(JsonValue root, int maxDepth) {
        this.path = new JsonValue[maxDepth];
        this.path[0] = root;
        this.index = 0;
//        this.marks = new int[20];   //TODO

        this.valueCursor = new JakartaValueCursor(this);
        this.arrayCursor = new JakartaArrayCursor(this);
        this.mapCursor = new JakartaMapCursor(this);
    }

    public JsonValue value() {
        if (index >= path.length) {
            throw new IndexOutOfBoundsException();
        }
        return path[index];
    }
    
    public boolean prev() {
        if (index == 0) {
            return false;
        }
        index--;
        return true;        
    }
    
    public MapCursor next(JsonObject value) {
        path[++index] = value;
        return mapCursor;
    }

    public ArrayCursor next(JsonArray value) {
        path[++index] = value;
        return arrayCursor;
    }

    public ValueCursor next(JsonValue value) {
        path[++index] = value;
        return valueCursor;
    }
    
    public ValueCursor cursor() {
        return valueCursor;
    }
    
    public MapCursor mapCursor() {
        return mapCursor;
    }
    
    public ArrayCursor arrayCursor() {
        return arrayCursor;
    }
    
    @Override
    public JakartaJsonCursor clone() {
        return new JakartaJsonCursor(value(), path.length);
    }
    
    @Override
    public String toString() {
       return new StringBuilder()
           .append(JakartaJsonCursor.class.getSimpleName())
           .append('[')
           .append("index=")
           .append(index)
           .append(", value=")
           .append(path[index])
           .append(']')
           .toString();
    }

    @Override
    public void mark() {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void reset() {
        // TODO Auto-generated method stub
        
    }
}
