package com.apicatalog.cborld.config;

import com.apicatalog.cborld.CborLdVersion;

public class BaseConfig implements Config {

//    protected DocumentLoader loader;
//    protected boolean bundledContexts;
//    protected URI base;

    protected BaseConfig() {
//        this.bundledContexts = bundledContexts;
//        this.loader = null;
//        this.base = null;
    }

//    @Override
//    public DocumentLoader loader() {
//        return loader;
//    }
//
//    @Override
//    public URI base() {
//        return base;
//    }

    @Override
    public CborLdVersion version() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean isCompactArrays() {
        // TODO Auto-generated method stub
        return false;
    }
}
