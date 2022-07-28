package com.apicatalog.cborld.encoder.value;

import java.util.UUID;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.json.cursor.JsonValueCursor;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.uuid.Uuid;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class UuidValueEncoder implements ValueEncoder {

    final static String PREFIX = "urn:uuid:";
    
    @Override
    public DataItem encode(Dictionary dictionary, JsonValueCursor value, String term, TermDefinition def) {

        if (value.isString() && value.stringValue().toLowerCase().startsWith(PREFIX)) {
            
            String rest = value.stringValue().substring(PREFIX.length());
            
            Array result = new Array();
                        
            result.add(new UnsignedInteger(3));
            result.add(new ByteString(Uuid.toBytes(UUID.fromString(rest))));
            
            return result;
        }
        return null;
    }
}
