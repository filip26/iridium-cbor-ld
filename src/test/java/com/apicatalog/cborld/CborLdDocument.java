package com.apicatalog.cborld;

import java.net.URI;
import java.util.Optional;

import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.media.MediaType;

class CborLdDocument implements Document {

    public static final MediaType MEDIA_TYPE = MediaType.of("application", "cbor");

    private final byte[] encoded;

    private CborLdDocument(byte[] encoded) {
        this.encoded = encoded;
    }

    public static final CborLdDocument of(byte[] cborLd) {
        return new CborLdDocument(cborLd);
    }

    @Override
    public MediaType getContentType() {
        return MEDIA_TYPE;
    }

    @Override
    public URI getContextUrl() {
        return null;
    }

    @Override
    public void setContextUrl(URI contextUrl) {
    }

    @Override
    public URI getDocumentUrl() {
        return null;
    }

    @Override
    public void setDocumentUrl(URI documentUrl) {
    }

    @Override
    public Optional<String> getProfile() {
        return Optional.empty();
    }

    public byte[] getByteArray() {
        return encoded;
    }
}
