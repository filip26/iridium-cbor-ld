package com.apicatalog.lq.cbor;

import java.util.function.Function;

import com.apicatalog.lq.Data;
import com.apicatalog.lq.Functions;

import co.nstant.in.cbor.model.DataItem;

public class CborAdapter {

    public static final Data of(DataItem value,
            Function<DataItem, String> dataToKey,
            Function<String, DataItem> keyToData) {
        return of(value, new CborFunctions(dataToKey, keyToData));
    }

    @SuppressWarnings("unchecked")
    public static final Data of(final DataItem value, final Functions fnc) {
        return CborFunctions.isNull(value)
                ? null
                : t -> ((Function<DataItem, ?>) t.apply(fnc)).apply(value);
    }
}
