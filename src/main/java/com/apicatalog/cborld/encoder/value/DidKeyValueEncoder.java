package com.apicatalog.cborld.encoder.value;

import java.net.URI;
import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.did.Did;
import com.apicatalog.did.DidUrl;
import com.apicatalog.did.key.DidKey;
import com.apicatalog.jsonld.StringUtils;
import com.apicatalog.multibase.MultibaseDecoder;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class DidKeyValueEncoder implements ValueEncoder {

    public final static String PREFIX = "did:key:";
    public final static int CODE = 1025;

    protected static MultibaseDecoder BASES = MultibaseDecoder.getInstance();
    
    @Override
    public DataItem encode(Mapping mapping, ValueCursor value, String term, Collection<String> types) {

        if (value.isString() && value.stringValue().toLowerCase().startsWith(PREFIX)) {

            try {
                
                final DidUrl did = DidUrl.from(URI.create(value.stringValue()));
                
                final DidKey key = DidKey.from(did, BASES);
                
                final Array result = new Array();
                
                result.add(new UnsignedInteger(CODE));
                result.add(new ByteString(key.getKey()));
                
                if (StringUtils.isNotBlank(did.getFragment())) {
                    
                    final DidKey fragment = DidKey.from(Did.from(PREFIX + did.getFragment()), BASES);
                    
                    result.add(new ByteString(fragment.getKey()));
                }

                return result;

            } catch (IllegalArgumentException e) {
                /*ignore */
            }
        }
        return null;
    }
}
