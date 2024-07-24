package com.apicatalog.cborld.config;

import java.net.URI;

import com.apicatalog.jsonld.loader.DocumentLoader;

public class BaseConfig implements Config {

    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected boolean compactArrays;
    protected URI base;

    protected BaseConfig(boolean bundledContexts, boolean compactArrays) {
        this.bundledContexts = bundledContexts;
        this.compactArrays = compactArrays;
        this.loader = null;
        this.base = null;
    }

    @Override
    public boolean isCompactArrays() {
        return compactArrays;
    }

    @Override
    public DocumentLoader loader() {
        return loader;
    }

    @Override
    public URI base() {
        return base;
    }
}
