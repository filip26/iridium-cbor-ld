package com.apicatalog.cborld.dictionary;

import java.math.BigInteger;

/**
 * 
 * 
 *
 */
public interface Dictionary {

    BigInteger getCode(String value);

    String getValue(BigInteger code);

}
