package com.apicatalog.cborld.decoder.value;

import java.util.Collection;
import java.util.List;

import com.apicatalog.cborld.decoder.DecoderError;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.encoder.value.DidKeyValueEncoder;
import com.apicatalog.cborld.hex.Hex;
import com.apicatalog.multibase.Multibase;
import com.apicatalog.multicodec.Multicodec;
import com.apicatalog.multicodec.Multicodec.Tag;
import com.apicatalog.multicodec.MulticodecDecoder;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonValue;

public class DidKeyValueDecoder implements ValueDecoder {

    protected static final MulticodecDecoder CODECS = MulticodecDecoder.getInstance(Tag.Key);
    
    @Override
    public JsonValue decode(Dictionary dictionary, DataItem value, String term, Collection<String> types) throws DecoderError {

        if (MajorType.ARRAY.equals(value.getMajorType())) {

            final List<DataItem> items = ((Array)value).getDataItems(); 
            
            if (items.size() < 2 || items.size() > 3) {
                return null;
            }
            
            final DataItem code = items.get(0);
            final DataItem part1 = items.get(1);
            final DataItem part2 = items.size() == 3 ? items.get(2) : null; 
            
            if (!MajorType.UNSIGNED_INTEGER.equals(code.getMajorType())
                    || DidKeyValueEncoder.CODE != ((UnsignedInteger)code).getValue().longValueExact()
                    || !MajorType.BYTE_STRING.equals(part1.getMajorType())
                    || (part2 != null && !MajorType.BYTE_STRING.equals(part2.getMajorType()))
                    ) {
                return null;
            }
            
            final String key = decode((ByteString)part1);
            
            final String fragment = part2 != null
                                ? "#" + decode((ByteString)part2)
                                : "";
            
            return Json.createValue(DidKeyValueEncoder.PREFIX +  key + fragment);
        }
        
        return null;
    }

    private String decode(ByteString dataItem) throws DecoderError  {
        
        byte[] bytes = dataItem.getBytes();
        
        byte[] codecBytes = new byte[] { bytes[0], bytes[1] };
        
        Multicodec codec = CODECS.getCodec(codecBytes).orElseThrow(() -> new DecoderError(Code.Unsupported, "Unknown DID key codec " + Hex.toString(codecBytes) + "."));
    
        byte[] rawKey = new byte[bytes.length - 2];
        System.arraycopy(bytes, 2, rawKey, 0, bytes.length - 2);
        
        byte[] encodedBytes = codec.encode(rawKey);
        
        String encoded = Multibase.BASE_58_BTC.encode(encodedBytes);
        
        return encoded;
    }
}
