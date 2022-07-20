package com.apicatalog.cborld.dictionary;

/**
 * 
 * 
 *
 */
public interface Dictionary {

    byte[] getCode(String term);

    String getTerm(byte[] value);

}
