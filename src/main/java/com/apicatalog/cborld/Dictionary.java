package com.apicatalog.cborld;

/**
 * 
 * 
 *
 */
public interface Dictionary {

    byte[] getCode(String term);

    String getTerm(byte[] value);
    
}
