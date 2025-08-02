package com.apicatalog.cborld.config;

import java.net.URI;

import com.apicatalog.jsonld.loader.DocumentLoader;

public class BaseConfig implements Config {

    protected DocumentLoader loader;
    protected boolean bundledContexts;
    protected URI base;

    protected BaseConfig(boolean bundledContexts) {
        this.bundledContexts = bundledContexts;
        this.loader = null;
        this.base = null;
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
