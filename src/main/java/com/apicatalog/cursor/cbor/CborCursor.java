package com.apicatalog.cursor.cbor;

import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.function.Function;

import com.apicatalog.cursor.ArrayCursor;
import com.apicatalog.cursor.ArrayItemCursor;
import com.apicatalog.cursor.Cursor;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.MapEntryCursor;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.UnsignedInteger;

public class CborCursor implements Cursor<DataItem> {

    @FunctionalInterface
    public interface ValueDecoder {
        DataItem decode(DataItem value, String term);
    }
    
    final Deque<DataItem> path;
    final Deque<Object> indices;
    
    final ArrayItemCursor arrayItemCursor;
    final MapEntryCursor mapEntryCursor;
    
    final MapCursor mapCursor;
    final ArrayCursor arrayCursor;
    
    final Function<String, DataItem> keyToCode;
    final ValueDecoder decodeValue;

    protected CborCursor(DataItem root, Function<DataItem, String> dataToKey, Function<String, DataItem> keyToData, ValueDecoder decodeValue) {
        
        this.path = new ArrayDeque<>();
        this.path.add(root);
        
        this.indices = new ArrayDeque<>();

        this.keyToCode = keyToData;
        this.decodeValue = decodeValue;

        this.arrayItemCursor = new CborArrayItemCursor(this);
        this.mapEntryCursor = new CborMapEntryCursor(this);
        
        this.arrayCursor = new CborArrayCursor(this);
        this.mapCursor = new CborMapCursor(this, dataToKey, keyToData);
    }
    
    public static MapCursor from(DataItem data, Function<DataItem, String> dataToKey, Function<String, DataItem> keyToData, ValueDecoder decodeValue) {
        return new CborCursor(data, dataToKey, keyToData, decodeValue).mapCursor();
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
        
        final Map map = (Map)path.peek(); 
        
        DataItem key = keyToCode.apply(mapKey);
        DataItem value = map.get(key);
        Boolean arrayCode = Boolean.FALSE;

        if (value == null && MajorType.UNSIGNED_INTEGER.equals(key.getMajorType())) {
            key = new UnsignedInteger(((UnsignedInteger)key).getValue().add(BigInteger.ONE));
            value = map.get(key);        
            if (value != null) {
                arrayCode = Boolean.TRUE;
            }   
        }
        
        if ((!arrayCode && MajorType.ARRAY.equals(value.getMajorType()))
                ) {

            value = decodeValue.decode(value, mapKey);
            
        } else if (MajorType.ARRAY.equals(value.getMajorType())) {
            
            Collection<DataItem> items = ((Array)value).getDataItems();
            
            Array newValues = new Array(items.size());
            
            for (DataItem item : items) {
                newValues.add(decodeValue.decode(item, mapKey));
            }
            
            value = newValues;
            
        } else {
            value = decodeValue.decode(value, mapKey);
        }

System.out.println("entry " + mapKey + ", " + value);
        path.push(value);
        
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
        path.pop();
        return entry(mapKey);
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
