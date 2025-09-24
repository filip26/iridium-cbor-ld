package com.apicatalog.cborld.context.mapping;

import java.math.BigInteger;
import java.util.Collection;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import com.apicatalog.tree.io.CborAdapter;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

class CborMapping extends CborAdapter {

    final Function<DataItem, String> decodeTerm;
    final Function<String, DataItem> encodeTerm;

    final BiFunction<DataItem, String, DataItem> decodeValue;

    CborMapping(
            Function<DataItem, String> decodeTerm,
            Function<String, DataItem> encodeTerm,
            BiFunction<DataItem, String, DataItem> decodeValue) {
        this.decodeTerm = decodeTerm;
        this.encodeTerm = encodeTerm;
        this.decodeValue = decodeValue;
    }

    @Override
    public DataItem property(Object property, Object node) {
        if (property instanceof String term) {
            DataItem key = encodeTerm.apply(term);
            return get(term, key, node);
        }
        if (isNumber(property) && property instanceof DataItem key) {
            String term = decodeTerm.apply(key);
            return get(term, key, node);
        }
        return super.property(property, node);
    }

    @Override
    public Stream<Entry<?, ?>> propertyStream(Object node) {
        return super.propertyStream(node).map(e -> new Entry<Object, Object>() {

            @Override
            public Object getKey() {
                if (e.getKey() instanceof DataItem keyCode) {
                    return decodeTerm.apply(keyCode);
                }
                return e.getKey();
            }

            @Override
            public Object getValue() {
                if (isNumber(e.getKey()) && e.getKey() instanceof DataItem key) {
                    String term = decodeTerm.apply(key);
                    return get(term, key, node);
                }
                return e.getValue();
            }

            @Override
            public Object setValue(Object value) {
                throw new UnsupportedOperationException();
            }
        });
    }

    @Override
    public String asString(Object node) {
        if (node instanceof DataItem data) {
            return decodeTerm.apply(data);
        }
        return super.asString(node);
    }

    protected DataItem get(String term, DataItem key, Object node) {
        boolean arrayCode = false;
        DataItem value = super.property(key, node);

        if (value == null && key instanceof UnsignedInteger keyCode) {
            key = new UnsignedInteger((keyCode).getValue().add(BigInteger.ONE));
            value = super.property(key, node);
            if (value != null) {
                arrayCode = true;
            }
        }

        if (value != null) {
            if ((!arrayCode && MajorType.ARRAY == value.getMajorType())) {

                value = decodeValue.apply(value, term);

            } else if (value instanceof Array array) {

                Collection<DataItem> items = array.getDataItems();

                Array newValues = new Array(items.size());

                for (DataItem item : items) {
                    newValues.add(decodeValue.apply(item, term));
                }

                value = newValues;

            } else {
                value = decodeValue.apply(value, term);
            }
            return value;
        }

        return super.property(new UnicodeString(term), node);
    }

}
