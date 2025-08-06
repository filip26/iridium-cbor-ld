package com.apicatalog.cborld.config;

import com.apicatalog.cborld.CborLdVersion;

public interface BaseConfig {

    CborLdVersion version();

    boolean isCompactArrays();    
}
