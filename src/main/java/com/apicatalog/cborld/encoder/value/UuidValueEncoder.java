package com.apicatalog.cborld.encoder.value;

import java.util.Collection;
import java.util.UUID;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.uuid.Uuid;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class UuidValueEncoder implements ValueEncoder {

    public final static String PREFIX = "urn:uuid:";
    public final static int CODE = 3;
    
    @Override
    public DataItem encode(Dictionary dictionary, ValueCursor value, String term, Collection<String> types) {

        if (value.isString() && value.stringValue().toLowerCase().startsWith(PREFIX)) {
            
            String rest = value.stringValue().substring(PREFIX.length());
            
            Array result = new Array();
                        
            result.add(new UnsignedInteger(CODE));
            result.add(new ByteString(Uuid.toBytes(UUID.fromString(rest))));
            
            return result;
        }
        return null;
    }
}
