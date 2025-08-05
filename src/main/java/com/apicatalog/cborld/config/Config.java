package com.apicatalog.cborld.config;

import com.apicatalog.cborld.CborLdVersion;

public interface Config {

    CborLdVersion version();

    boolean isCompactArrays();    
}
