package com.apicatalog.cborld.config;

import java.net.URI;

import com.apicatalog.jsonld.loader.DocumentLoader;

public interface Config {

    boolean isCompactArrays();

    DocumentLoader loader();

    URI base();
}
