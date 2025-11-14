package com.apicatalog.cborld;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.loader.DocumentLoader;

@Deprecated
class ClasspathLoader  {
    //implements DocumentLoader
//
//    @Override
//    public Document loadDocument(URI url, Options options) throws JsonLdException {
//
//        try (final InputStream is = getClass().getResourceAsStream(url.getSchemeSpecificPart())) {
//
//            if (is == null) {
//                throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED);
//            }
//
//            final Document document = toDocument(url, is);
//            document.setDocumentUrl(url);
//
//            return document;
//
//        } catch (IOException e) {
//            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED);
//        }
//    }
//
//    private static final Document toDocument(URI url, InputStream is) throws JsonLdException {
//
////        if (url.toString().endsWith(".nq")) {
////            return RdfDocument.of(is);
////
////        } else if (url.toString().endsWith(".cborld")) {
////            try {
////                return CborLdDocument.of(is.readAllBytes());
////            } catch (IOException e) {
////                throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, e);
////            }
////        }
//
//        return Document.of(is);
//    }
}
