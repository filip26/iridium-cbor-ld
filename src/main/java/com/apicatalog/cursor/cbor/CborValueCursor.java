package com.apicatalog.cursor.cbor;

import java.util.function.Supplier;

import com.apicatalog.cursor.AbstractValueCursor;
import com.apicatalog.cursor.Cursor;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.SimpleValue;
import co.nstant.in.cbor.model.Special;
import co.nstant.in.cbor.model.SpecialType;
import co.nstant.in.cbor.model.UnicodeString;

public class CborValueCursor extends AbstractValueCursor<DataItem> {

    public CborValueCursor(Cursor<DataItem> cursor, Supplier<DataItem> value) {
        super(cursor, value);
    }

    @Override
    public boolean isNull() {
        return value.get() == null;
    }

    @Override
    public boolean isString() {
        return value.get() != null && MajorType.UNICODE_STRING.equals(value.get().getMajorType());
    }

    @Override
    public String stringValue() {
        return ((UnicodeString)value.get()).getString();
    }

    @Override
    public boolean isBoolean() {
        return value.get() != null
                && MajorType.SPECIAL.equals(value.get().getMajorType())
                && SpecialType.SIMPLE_VALUE.equals(((Special)value.get()).getSpecialType())
                && SimpleValue.TRUE.equals(value.get())
                && SimpleValue.FALSE.equals(value.get())
                ;
    }

    @Override
    public Boolean booleanValue() {
        if (SimpleValue.TRUE.equals(value.get())) {
            return true;
        }
        if (SimpleValue.FALSE.equals(value.get())) {
            return false;
        }

        throw new ClassCastException();
    }

    @Override
    public boolean isNumber() {
        return value.get() != null &&
                (MajorType.UNSIGNED_INTEGER.equals(value.get().getMajorType())
                   ||     MajorType.NEGATIVE_INTEGER.equals(value.get().getMajorType())       
                   );
        
    }

    @Override
    public Integer integerValue() {
        return ((co.nstant.in.cbor.model.Number)value.get()).getValue().intValueExact();            
    }

    @Override
    public Long longValue() {
        return ((co.nstant.in.cbor.model.Number)value.get()).getValue().longValueExact();
    }

    @Override
    public boolean isArray() {
        return value.get() != null && MajorType.ARRAY.equals(value.get().getMajorType());
    }

    @Override
    public boolean isMap() {
        return value.get() != null && MajorType.MAP.equals(value.get().getMajorType());
    }
}
