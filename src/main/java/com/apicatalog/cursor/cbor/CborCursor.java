package com.apicatalog.cursor.cbor;

import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.function.Function;
import java.util.stream.Collectors;

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
        DataItem decode(DataItem value, String term, Collection<String> path);
    }

    final Deque<CborCursorState> stack;
    
    final ArrayItemCursor arrayItemCursor;
    final MapEntryCursor mapEntryCursor;
    
    final MapCursor mapCursor;
    final ArrayCursor arrayCursor;
    
    final Function<String, DataItem> keyToCode;
    final ValueDecoder decodeValue;

    protected CborCursor(DataItem root, Function<DataItem, String> dataToKey, Function<String, DataItem> keyToData, ValueDecoder decodeValue) {
        
        this.stack = new ArrayDeque<>();
        this.stack.add(new CborCursorState(root, null, null));
        
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
        if (stack.isEmpty()) {
            throw new IndexOutOfBoundsException();
        }
        return stack.peek().data();
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
    public MapEntryCursor entry(final String mapKey) {
        
        final Map map = (Map)stack.peek().data(); 
        
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
        
        if (value != null) {
            final Collection<String> path = stack.stream()
                    .filter(ss -> ss.key() != null)
                    .map(ss -> ss.key())
                    .collect(Collectors.toList());
    
            if ((!arrayCode && MajorType.ARRAY.equals(value.getMajorType()))
                    ) {
    
                value = decodeValue.decode(value, mapKey, path);
                
            } else if (MajorType.ARRAY.equals(value.getMajorType())) {
                
                Collection<DataItem> items = ((Array)value).getDataItems();
                
                Array newValues = new Array(items.size());
                
                for (DataItem item : items) {
                    newValues.add(decodeValue.decode(item, mapKey, path));
                }
                
                value = newValues;
                
            } else {
                value = decodeValue.decode(value, mapKey, path);
            }
        }

        stack.push(new CborCursorState(value, null, mapKey));
        
        return mapEntryCursor;
    }

    // horizontal
    @Override
    public ArrayItemCursor item(int arrayIndex) {
        stack.push(new CborCursorState(((Array)stack.peek().data()).getDataItems().get(arrayIndex), arrayIndex, null));
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
        return stack.peek().index();
    }

    @Override
    public String key() {
        return stack.peek().key();
    }

    @Override
    public String toString() {
       return new StringBuilder()
           .append(getClass().getSimpleName())
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
        return !stack.isEmpty() && (stack.peek().index() != null);
    }
    
    @Override    
    public boolean isMapEntry() {
        return !stack.isEmpty() && (stack.peek().key() != null);
    }
}
