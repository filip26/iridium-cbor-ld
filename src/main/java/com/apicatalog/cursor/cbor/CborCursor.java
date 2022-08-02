package com.apicatalog.cursor.cbor;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.function.Function;

import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.ArrayItemCursor;
import com.apicatalog.cursor.Cursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.MapEntryCursor;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;

public class CborCursor implements Cursor<DataItem> {

    final Deque<DataItem> path;
    final Deque<Object> indices;
    
    final ArrayItemCursor arrayItemCursor;
    final MapEntryCursor mapEntryCursor;
    
    final MapCursor mapCursor;
    final ArrayCursor arrayCursor;
    
    final Function<String, DataItem> keyToData;

    protected CborCursor(DataItem root, Function<DataItem, String> dataToKey, Function<String, DataItem> keyToData) {
        
        this.path = new ArrayDeque<>();
        this.path.add(root);
        
        this.indices = new ArrayDeque<>();

        this.keyToData = keyToData;

        this.arrayItemCursor = new CborArrayItemCursor(this);
        this.mapEntryCursor = new CborMapEntryCursor(this);
        
        this.arrayCursor = new CborArrayCursor(this);
        this.mapCursor = new CborMapCursor(this, dataToKey, keyToData);
    }
    
    public static MapCursor from(DataItem data, Function<DataItem, String> dataToKey, Function<String, DataItem> keyToData) {
        return new CborCursor(data, dataToKey, keyToData).mapCursor();
    }

    @Override
    public DataItem sourceValue() {
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
        indices.push(mapKey);
        System.out.println(">>> " + mapKey + ", " + keyToData.apply(mapKey) + ", " + path.peek());
        path.push(((Map)path.peek()).get(keyToData.apply(mapKey)));
        return mapEntryCursor;
    }

    // horizontal
    @Override
    public ArrayItemCursor item(int arrayIndex) {
        indices.push(arrayIndex);
        path.push(((Array)path.peek()).getDataItems().get(arrayIndex));
        return arrayItemCursor;
    }

    // vertical
    @Override
    public MapEntryCursor mapKey(String mapKey) {
        indices.pop();
        indices.push(mapKey);

        path.pop();
        path.push(((Map)path.peek()).get(keyToData.apply(mapKey)));

        return mapEntryCursor;
    }

    // vertical
    @Override
    public ArrayItemCursor arrayIndex(int arrayIndex) {
        indices.pop();
        indices.push(arrayIndex);

        path.pop();
        path.push(((Array)path.peek()).getDataItems().get(arrayIndex));

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
           .append(getClass().getSimpleName())
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
