package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.multibase.Multibase;

import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;

public class MultibaseValueEncoder implements ValueEncoder {
    
    static final String TYPE = "https://w3id.org/security#multibase";
    
    @Override
    public DataItem encode(Dictionary dictionary, ValueCursor value, String term, Collection<String> types) {

        if (value.isString() && types != null && types.contains(TYPE)) {

            byte[] bytes = Multibase.decode(value.stringValue());
            if (bytes != null) {
                
                byte[] compressed = new byte[bytes.length + 1];
                
                compressed[0] = 'z';
                
                System.arraycopy(bytes, 0, compressed, 1, bytes.length);
                
                return new ByteString(compressed);
            }            
        }
        return null;
    }
}
