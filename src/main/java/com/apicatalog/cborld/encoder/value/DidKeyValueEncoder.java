package com.apicatalog.cborld.encoder.value;

import java.util.Collection;

import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.multicodec.Multicodec.Tag;
import com.apicatalog.multicodec.MulticodecDecoder;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class DidKeyValueEncoder implements ValueEncoder {

    public final static String PREFIX = "did:key:";
    public final static int CODE = 1025;

    protected static MulticodecDecoder CODECS = MulticodecDecoder.getInstance(Tag.Key);
    
    @Override
    public DataItem encode(Mapping mapping, ValueCursor value, String term, Collection<String> types) {

        if (value.isString() && value.stringValue().toLowerCase().startsWith(PREFIX)) {

            try {
                
//                final DidUrl did = DidUrl.of(value.stringValue());
//                
//                final DidKey key = DidKey.of(did, CODECS);
                
                final Array result = new Array();
                
                result.add(new UnsignedInteger(CODE));
//                result.add(new ByteString(key.codec().encode(key.rawBytes())));

//                if (StringUtils.isNotBlank(did.getFragment())) {
//                    final DidKey fragment = DidKey.of(Did.of(PREFIX + did.getFragment()), CODECS);                    
//                    result.add(new ByteString(fragment.codec().encode(fragment.rawBytes())));
//                }

                return result;

            } catch (IllegalArgumentException e) {
                /*ignore */
            }
        }
        return null;
    }
}
