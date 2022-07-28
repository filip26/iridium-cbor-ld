package com.apicatalog.cborld.encoder.value;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.json.cursor.JsonValueCursor;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.multibase.Multibase;

import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;

public class MultibaseValueEncoder implements ValueEncoder {
    
    static final String TYPE = "https://w3id.org/security#multibase";
    
    @Override
    public DataItem encode(Dictionary dictionary, JsonValueCursor value, String term, TermDefinition def) {

        if (value.isString() && def != null && TYPE.equals(def.getTypeMapping())) {

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
