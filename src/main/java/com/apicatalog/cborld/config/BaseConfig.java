package com.apicatalog.cborld.config;

import com.apicatalog.cborld.CborLd.Version;

public interface BaseConfig {

    Version version();

    boolean isCompactArrays();    
}
