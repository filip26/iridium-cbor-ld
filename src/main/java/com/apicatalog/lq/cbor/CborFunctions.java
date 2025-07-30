package com.apicatalog.lq.cbor;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.apicatalog.lq.Data;
import com.apicatalog.lq.DataType;
import com.apicatalog.lq.Functions;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.DoublePrecisionFloat;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.Number;
import co.nstant.in.cbor.model.SimpleValue;
import co.nstant.in.cbor.model.Special;
import co.nstant.in.cbor.model.SpecialType;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

public class CborFunctions implements Functions {

    final Function<DataItem, String> dataToKey;
    final Function<String, DataItem> keyToData;

    public CborFunctions(
            Function<DataItem, String> dataToKey,
            Function<String, DataItem> keyToData) {
        this.dataToKey = dataToKey;
        this.keyToData = keyToData;
    }

    @Override
    public Function<Map, Boolean> contains(String key) {
        return value -> {
            DataItem item = keyToData.apply(key);

            if (value.getKeys().contains(item)) {
                return true;
            }

            if (MajorType.UNSIGNED_INTEGER.equals(item.getMajorType())) {
                item = new UnsignedInteger(((UnsignedInteger) item).getValue().add(BigInteger.ONE));
                return (value.getKeys().contains(item));
            }
            return false;
        };
    }

    @Override
    public Function<Map, Data> value(String key) {
        return object -> {
            DataItem item = keyToData.apply(key);

            if (MajorType.UNSIGNED_INTEGER.equals(item.getMajorType())) {
                item = new UnsignedInteger(((UnsignedInteger) item).getValue().add(BigInteger.ONE));
            }
            return CborAdapter.of(object.get(item), this);
        };
    }

    @Override
    public Function<UnicodeString, String> getString() {
        return UnicodeString::getString;
    }

    @Override
    public Function<DataItem, Boolean> isEmpty() {
        return CborFunctions::isEmpty;
    }

    public static boolean isNull(DataItem value) {
        return value == null
                || (MajorType.SPECIAL.equals(value.getMajorType())
                        && SpecialType.SIMPLE_VALUE.equals(((Special) value).getSpecialType())
                        && SimpleValue.NULL.equals(((SimpleValue) value)));
    }

    public static final boolean isEmpty(DataItem value) {
//        return value.getValueType() == ValueType.OBJECT
//                ? value.asJsonObject().isEmpty()
//                : value.asJsonArray().isEmpty();
        return true;
    }

    @Override
    public Function<DataItem, Integer> size() {
        return (x) -> -1;
//        return value -> value.getValueType() == ValueType.OBJECT
//                ? value.asJsonObject().size()
//                : value.asJsonArray().size();
    }

    @Override
    public Function<Map, Collection<String>> keys() {
        return value -> value.getKeys().stream().map(dataToKey).collect(Collectors.toList());
    }

    @Override
    public Function<Array, Iterable<Data>> iterable() {
        return value -> value.getDataItems().stream().map(d -> CborAdapter.of(d, this)).toList();
    }

    @Override
    public Function<co.nstant.in.cbor.model.Number, BigInteger> getInteger() {
        return Number::getValue;
    }

    @Override
    public Function<DoublePrecisionFloat, BigDecimal> getDecimal() {
        return value -> BigDecimal.valueOf(value.getValue());
    }

    @Override
    public Function<DataItem, DataType> type() {
        return value -> {
            switch (value.getMajorType()) {
            case UNICODE_STRING:
                return DataType.STRING;

            case MAP:
                return DataType.MAP;

            case ARRAY:
                return DataType.ARRAY;

            case SPECIAL:
                if (SpecialType.SIMPLE_VALUE.equals(((Special) value).getSpecialType())) {
                    if (SimpleValue.TRUE.equals(value)) {
                        return DataType.TRUE;
                    }
                    if (SimpleValue.FALSE.equals(value)) {
                        return DataType.FALSE;
                    }
                    if (SimpleValue.NULL.equals(value)) {
                        return null;
                    }

                } else if (SpecialType.IEEE_754_DOUBLE_PRECISION_FLOAT.equals(((Special) value).getSpecialType())) {
                    return DataType.DECIMAL;
                }

            case UNSIGNED_INTEGER:
            case NEGATIVE_INTEGER:
                return DataType.INTEGER;

            default:
                break;
            }

            throw new IllegalStateException();
        };
    }

}
