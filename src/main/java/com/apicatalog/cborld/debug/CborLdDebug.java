package com.apicatalog.cborld.debug;

import java.net.URI;
import java.util.Iterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

public class CborLdDebug {

    final DocumentLoader loader;
    final URI base;

    protected CborLdVersion version;
    protected DocumentDictionary dictionary;
    protected Mapping mapping;

    protected JsonValue decoded;
    protected byte[] encoded;

    protected Exception error;

    public CborLdDebug(DocumentLoader loader, URI base) {
        this.loader = loader;
        this.base = base;

        this.version = null;
        this.dictionary = null;
        this.mapping = null;

        this.decoded = null;
        this.encoded = null;

        this.error = null;
    }

    public CborLdVersion version() {
        return version;
    }

    public DocumentDictionary dictionary() {
        return dictionary;
    }

    public Dictionary terms() {
        return mapping != null ? mapping.termMap() : null;
    }

    public boolean isCborLd() {
        return version != null && dictionary != null;
    }

    public boolean isError() {
        return error != null;
    }

    public Exception error() {
        return error;
    }

    public byte[] encoded() {
        return encoded;
    }

    public JsonValue decoded() {
        return decoded;
    }

    public JsonObject dump() {
        return Json.createObjectBuilder()
                .add("version", version != null ? Json.createValue(version.name()) : JsonValue.NULL)
                .add("dictionary", dictionary != null ? dump(dictionary) : JsonValue.NULL)

                .add("terms", mapping != null && mapping.termMap() != null
                        ? dump(mapping.termMap())
                        : JsonValue.NULL)
                .build();
    }

    static final JsonObject dump(DocumentDictionary dictionary) {
        var builder = Json.createObjectBuilder()
                .add("code", dictionary.code())
                .add("context", dictionary.contexts() != null
                        ? dump(dictionary.contexts())
                        : JsonValue.NULL);

        if (dictionary.types() != null) {
            dictionary.types().entrySet().forEach(typeEntry -> builder.add(typeEntry.getKey(), dump(typeEntry.getValue())));
        }
        if (dictionary.uris() != null) {
            builder.add("uri", dump(dictionary.uris()));
        }

        return builder.build();
    }

    static final JsonObject dump(Dictionary dictionary) {
        var builder = Json.createObjectBuilder();

        toStream(dictionary.iterator())
                .sorted((o1, o2) -> o1.getValue() - o2.getValue())
                .forEach(e -> builder.add(e.getKey(), e.getValue()));

        return builder.build();
    }

    static <T> Stream<T> toStream(Iterator<T> iterator) {
        return StreamSupport.stream(
                Spliterators.spliteratorUnknownSize(iterator, 0),
                false);
    }
}
