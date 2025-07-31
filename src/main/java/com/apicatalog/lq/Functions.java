package com.apicatalog.lq;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.function.Function;

public interface Functions {

    Function<?, DataType> type();
    
    Function<?, Boolean> isEmpty();

    Function<?, String> getString();

    Function<?, BigInteger> getInteger();

    Function<?, BigDecimal> getDecimal();
    
    Function<?, byte[]> getBinary();

    Function<?, Boolean> contains(String key);

    Function<?, Data> value(String key);

    Function<?, Iterable<Data>> iterable();

    Function<?, Integer> size();

    Function<?, Collection<String>> keys();
}
