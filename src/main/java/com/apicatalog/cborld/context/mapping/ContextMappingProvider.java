package com.apicatalog.cborld.context.mapping;

import java.math.BigInteger;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import com.apicatalog.cborld.context.Context;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.context.ContextError.Code;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.dictionary.CodeTermMap;
import com.apicatalog.cborld.document.DocumentDictionary;
import com.apicatalog.cborld.encoder.EncoderConfig;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
import com.apicatalog.cborld.mapping.EncoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cursor.MapCursor;
import com.apicatalog.cursor.cbor.CborCursor;
import com.apicatalog.jsonld.JsonLdError;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;
import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

public class ContextMappingProvider implements EncoderMappingProvider, DecoderMappingProvider {

    @Override
    public Mapping getEncoderMapping(JsonObject document, EncoderConfig config) throws ContextError {
        try {
            final Context context = Context.from(document, config.base(), config.loader());

            return new EncoderContextMapping(
                    config.dictionary().contexts(),
                    config.dictionary().types(),
                    CodeTermMap.of(context.getContextKeySets()),
                    context.getTypeMapping());

        } catch (JsonLdError e) {
            throw new ContextError(Code.InvalidContext, e);
        }
    }

    @Override
    public Mapping getDecoderMapping(DataItem document, DocumentDictionary custom, DecoderConfig config) throws ContextError {
        try {
            final DecoderContextMapping mapping2 = new DecoderContextMapping(custom.contexts(), custom.types(), config.valueDecoders());

          final MapCursor cursor = CborCursor.from(
          document,
          mapping2::decodeKey,
          mapping2::encodeKey,
          mapping2::decodeValue);

          System.out.println(":: " + cursor);
          System.out.println("::: " + mapping2.typeKeyNameMap());

          final DecoderContextMapping mapping = new DecoderContextMapping(custom.contexts(), custom.types(), config.valueDecoders());

          
            final JsonValue json = toValue(document, mapping);
            System.out.println(json);
            final Context context = Context.from(json, config.base(), config.loader(), mapping::add, mapping.typeKeyNameMap());

            mapping.typeMap(context.getTypeMapping());

            return mapping;

        } catch (JsonLdError e) {
            throw new ContextError(Code.InvalidContext, e);
        }
    }
    
    static final JsonValue toValue(DataItem dataItem, DecoderContextMapping mapping) {
        System.out.println(">>> " + dataItem + ", " + dataItem.getMajorType());

        switch (dataItem.getMajorType()) {
        case MAP:
            return toObject((co.nstant.in.cbor.model.Map)dataItem, mapping);
            
        case ARRAY:
            return toArray((Array)dataItem, mapping);
            
        case BYTE_STRING:
            return Json.createValue(new String(((ByteString)dataItem).getBytes()));
            
        case UNICODE_STRING:
            return Json.createValue(((UnicodeString)dataItem).getString());
            
        case UNSIGNED_INTEGER:
            return Json.createValue(((UnsignedInteger)dataItem).getValue());
        }
        
        
//        if (document.getMajorType() == MajorType.MAP) {
//            DataItem contextKey = mapping.encodeKey(Keywords.CONTEXT);
//            
//            DataItem contextValue = ((co.nstant.in.cbor.model.Map)document).get(contextKey);
//            
//            if (contextValue != null) {
//                
//                
//                if (contextValue.getMajorType() == MajorType.BYTE_STRING) {
//                    
//                }
//                System.out.println(">>> " + contextKey + ", " + contextValue);
//            }
//            
//        }
//        
        throw new IllegalStateException();
        //FIXME
        
    }
    
    static final JsonObject toObject(co.nstant.in.cbor.model.Map map, DecoderContextMapping mapping) {
        
        if (map.getKeys().isEmpty()) {
            return JsonValue.EMPTY_JSON_OBJECT;
        }
        
        final JsonObjectBuilder builder = Json.createObjectBuilder();
        
        for (DataItem key : map.getKeys()) {


//            DataItem key = keyToCode.apply(mapKey);
            DataItem value = map.get(key);
            Boolean arrayCode = Boolean.FALSE;

            if (value == null && MajorType.UNSIGNED_INTEGER.equals(key.getMajorType())) {
                key = new UnsignedInteger(((UnsignedInteger)key).getValue().add(BigInteger.ONE));
                value = map.get(key);        
                if (value != null) {
                    arrayCode = Boolean.TRUE;
                }   
            }


            String mapKey = mapping.decodeKey(key);

            
            if (value != null) {
                final Collection<String> path =
                        List.of(mapKey);
                        //stack.stream()
//                        .filter(ss -> ss.key() != null)
//                        .map(ss -> ss.key())
//                        .collect(Collectors.toList());
        
                if ((!arrayCode && MajorType.ARRAY.equals(value.getMajorType()))
                        ) {
        
                    value = mapping.decodeValue(value, mapKey, path);
                    
                } else if (MajorType.ARRAY.equals(value.getMajorType())) {
                    
                    Collection<DataItem> items = ((Array)value).getDataItems();
                    
                    Array newValues = new Array(items.size());
                    
                    for (DataItem item : items) {
                        newValues.add(mapping.decodeValue(item, mapKey, path));
                    }
                    
                    value = newValues;
                    
                } else {
                    value = mapping.decodeValue(value, mapKey, path);
                }
            }
            System.out.println("... " + key + ", " + mapKey + ", " + value);
//            stack.push(new CborCursorState(value, null, mapKey));

            
            
            builder.add(mapKey, toValue(value, mapping));
        }
    
        return builder.build();
    }
    
    static final JsonArray toArray(Array array, DecoderContextMapping mapping) {
        
        if (array.getDataItems().isEmpty()) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }
        
        final JsonArrayBuilder builder = Json.createArrayBuilder();
        
        for (final DataItem item : array.getDataItems()) {
            builder.add(toValue(item, mapping));
        }
        
        return builder.build();
    }
    

}
