package com.apicatalog.lq;

import java.util.function.Function;

@SuppressWarnings("rawtypes")
@FunctionalInterface
public interface ValueHolder extends Function<Function<Functions, Function>, Object> {
        
}
