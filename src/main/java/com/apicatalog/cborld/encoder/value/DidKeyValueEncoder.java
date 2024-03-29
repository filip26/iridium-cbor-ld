package com.apicatalog.cborld.encoder.value;

import java.net.URI;
import java.util.Collection;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cursor.ValueCursor;
import com.apicatalog.did.Did;
import com.apicatalog.did.DidUrl;
import com.apicatalog.did.key.DidKey;
import com.apicatalog.jsonld.StringUtils;
import com.apicatalog.multibase.MultibaseDecoder;
import com.apicatalog.multicodec.Multicodec.Tag;
import com.apicatalog.multicodec.MulticodecDecoder;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.UnsignedInteger;

public class DidKeyValueEncoder implements ValueEncoder {

    public final static String PREFIX = "did:key:";
    public final static int CODE = 1025;

    protected static MultibaseDecoder BASES = MultibaseDecoder.getInstance();
    protected static MulticodecDecoder CODECS = MulticodecDecoder.getInstance(Tag.Key);
    
    @Override
    public DataItem encode(Dictionary dictionary, ValueCursor value, String term, Collection<String> types) {

        if (value.isString() && value.stringValue().toLowerCase().startsWith(PREFIX)) {

            try {
                
                DidUrl did = DidUrl.from(URI.create(value.stringValue()));
                
                DidKey key = DidKey.from(did, BASES, CODECS);
                
                Array result = new Array();
                
                result.add(new UnsignedInteger(CODE));
                result.add(concatenate(key.getCodec().varint(), key.getRawKey()));
                
                if (StringUtils.isNotBlank(did.getFragment())) {
                    
                    DidKey fragment = DidKey.from(Did.from(PREFIX + did.getFragment()), BASES, CODECS);
                    
                    result.add(concatenate(fragment.getCodec().varint(), fragment.getRawKey()));
                }

                return result;

            } catch (IllegalArgumentException e) {
                /*ignore */
            }
        }
        return null;
    }
    
    final static ByteString concatenate(byte[] codec, byte[] key) {
        byte[] bytes = new byte[codec.length + key.length];
        System.arraycopy(codec, 0, bytes, 0, codec.length);
        System.arraycopy(key, 0, bytes, codec.length, key.length);
        return new ByteString(bytes);
    }
}
