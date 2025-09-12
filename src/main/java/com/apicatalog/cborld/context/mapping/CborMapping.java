package com.apicatalog.cborld.context.mapping;

import java.math.BigInteger;
import java.util.Collection;
import java.util.function.BiFunction;
import java.util.function.Function;

import com.apicatalog.tree.io.CborAdapter;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnsignedInteger;

public class CborMapping extends CborAdapter {

    protected final Function<DataItem, String> dataToTerm;
    protected final Function<String, DataItem> termToData;
    protected final BiFunction<DataItem, String, DataItem> decodeValue;
    
    protected CborMapping(
            Function<DataItem, String> dataToTerm,
            Function<String, DataItem> termToData,
            BiFunction<DataItem, String, DataItem> decodeValue) {
        this.dataToTerm = dataToTerm;
        this.termToData = termToData;
        this.decodeValue = decodeValue;
    }

    @Override
    public DataItem property(Object property, Object node) {        
        if (property instanceof String term) {
            
            DataItem key = termToData.apply(term);
            DataItem value = super.property(key, node);
            boolean arrayCode = false;

            if (value == null && MajorType.UNSIGNED_INTEGER.equals(key.getMajorType())) {
                key = new UnsignedInteger(((UnsignedInteger) key).getValue().add(BigInteger.ONE));
                value = super.property(key, node);
                if (value != null) {
                    arrayCode = true;
                }
            }
            if (value != null) {
//FIXME                
//                final Collection<String> path = stack.stream()
//                        .filter(ss -> ss.key() != null)
//                        .map(ss -> ss.key())
//                        .collect(Collectors.toList());

                if ((!arrayCode && MajorType.ARRAY.equals(value.getMajorType()))) {

                    value = decodeValue.apply(value, term);

                } else if (MajorType.ARRAY.equals(value.getMajorType())) {

                    Collection<DataItem> items = ((Array) value).getDataItems();

                    Array newValues = new Array(items.size());

                    for (DataItem item : items) {
                        newValues.add(decodeValue.apply(item, term));
                    }

                    value = newValues;

                } else {
                    value = decodeValue.apply(value, term);
                }
            }
            return value;
        }
        return super.property(property, node);
    }
    
    @Override
    public String stringValue(Object node) {
        return dataToTerm.apply((DataItem)node);
    }

}
