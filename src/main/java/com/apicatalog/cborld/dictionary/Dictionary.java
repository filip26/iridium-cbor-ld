package com.apicatalog.cborld.dictionary;

/**
 *
 *
 *
 */
public interface Dictionary {

    Integer getCode(String value);

    String getValue(Integer code);

}
