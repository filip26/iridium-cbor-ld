package com.apicatalog.cursor.cbor;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.Supplier;

import com.apicatalog.cursor.AbstractValueCursor;
import com.apicatalog.cursor.Cursor;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.DoublePrecisionFloat;
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
    public boolean isScalar() {
        return super.isScalar() 
                || (value.get() != null && MajorType.BYTE_STRING.equals(value.get().getMajorType()));
    }
    
    @Override
    public boolean isNull() {
        return value.get() == null || 
                (MajorType.SPECIAL.equals(value.get().getMajorType())
                        && SpecialType.SIMPLE_VALUE.equals(((Special)value.get()).getSpecialType())
                        && SimpleValue.NULL.equals(((SimpleValue)value.get()))
                        );
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
                && (SimpleValue.TRUE.equals(value.get())
                        || SimpleValue.FALSE.equals(value.get())
                        )
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
        return isInteger() || isDecimal();
    }
    
    @Override
    public boolean isInteger() {
        return value.get() != null
                && (MajorType.UNSIGNED_INTEGER.equals(value.get().getMajorType())
                        || MajorType.NEGATIVE_INTEGER.equals(value.get().getMajorType())       
                   );
    }
    
    @Override
    public boolean isDecimal() {
        return value.get() != null 
                && MajorType.SPECIAL.equals(value.get().getMajorType())
                && SpecialType.IEEE_754_DOUBLE_PRECISION_FLOAT.equals(((Special)value.get()).getSpecialType())       
               ;
    }

    @Override
    public BigInteger integerValue() {
        return ((co.nstant.in.cbor.model.Number)value.get()).getValue();         
    }

    @Override
    public BigDecimal decimalValue() {
        return BigDecimal.valueOf(((DoublePrecisionFloat)value.get()).getValue());
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
