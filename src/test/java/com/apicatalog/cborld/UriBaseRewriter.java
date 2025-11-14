package com.apicatalog.cborld;

import java.net.URI;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.loader.DocumentLoader;

final class UriBaseRewriter implements DocumentLoader {

    private final String sourceBase;
    private final String targetBase;

    private final DocumentLoader loader;

    public UriBaseRewriter(final String sourceBase, final String targetBase, final DocumentLoader loader) {
        this.sourceBase = sourceBase;
        this.targetBase = targetBase;

        this.loader = loader;
    }

    @Override
    public Document loadDocument(final URI url, final Options options) throws JsonLdException {

        final String sourceUrl = url.toString();

        if (!sourceUrl.startsWith(sourceBase)) {
            return loader.loadDocument(url, options);
        }

        final String relativePath = sourceUrl.substring(sourceBase.length());

        final Document remoteDocument = loader.loadDocument(URI.create(targetBase + relativePath), options);

        if (remoteDocument == null) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED);
        }
//TODO
//        if (remoteDocument.getDocumentUrl() != null && remoteDocument.getDocumentUrl().toString().startsWith(targetBase)) {
//
//            final String remoteRelativePath = remoteDocument.getDocumentUrl().toString().substring(targetBase.length());
//            remoteDocument.setDocumentUrl(URI.create(sourceBase + remoteRelativePath));
//        }

        return remoteDocument;
    }
}
