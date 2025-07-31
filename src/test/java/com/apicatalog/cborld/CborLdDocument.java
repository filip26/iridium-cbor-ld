package com.apicatalog.cborld;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Optional;

import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.media.MediaType;

public class CborLdDocument implements Document {

    public static final MediaType MEDIA_TYPE = MediaType.of("application", "cbor");

    private final byte[] encoded;

    private CborLdDocument(byte[] encoded) {
        this.encoded = encoded;
    }

    public static final CborLdDocument from(InputStream is) throws IOException {
        return from(readAllBytes(is));
    }

    public static final CborLdDocument from(byte[] cborLd) {
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

    public static byte[] readAllBytes(InputStream inputStream) throws IOException {
        final int bufLen = 4 * 0x400; // 4KB
        byte[] buf = new byte[bufLen];
        int readLen;
        IOException exception = null;

        try {
            try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {

                while ((readLen = inputStream.read(buf, 0, bufLen)) != -1) {
                    outputStream.write(buf, 0, readLen);
                }

                return outputStream.toByteArray();
            }

        } catch (IOException e) {
            exception = e;
            throw e;

        } finally {
            if (exception == null) {
                inputStream.close();
            } else {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    exception.addSuppressed(e);
                }
            }
        }
    }

    public byte[] getByteArray() {
        return encoded;
    }
}
