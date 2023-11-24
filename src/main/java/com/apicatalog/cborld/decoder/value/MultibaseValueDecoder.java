package com.apicatalog.cborld.decoder.value;

import java.util.Collection;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.encoder.value.MultibaseValueEncoder;
import com.apicatalog.multibase.Multibase;

import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class MultibaseValueDecoder implements ValueDecoder {
    
    
    @Override
    public JsonValue decode(Dictionary dictionary, DataItem value, String term, Collection<String> types) throws DecoderError {

        if (types != null && types.contains(MultibaseValueEncoder.TYPE)
                && MajorType.BYTE_STRING.equals(value.getMajorType())
                ) {

            byte[] byteString = ((ByteString)value).getBytes();
            
            if (byteString.length <= 1 || byteString[0] != 'z') {
                return null;
            }
            
            
            byte[] data = new byte[byteString.length - 1];
            System.arraycopy(byteString, 1, data, 0, byteString.length - 1);
            
            String encoded = Multibase.BASE_58_BTC.encode(data);

            if (encoded != null) {
                return Json.createValue(encoded);
            }
        }
        return null;
    }
}
