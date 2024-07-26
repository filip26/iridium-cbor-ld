package com.apicatalog.cborld.document;

import com.apicatalog.cborld.dictionary.ContextDictionary;

public class CompressedDocument {

    public static final int CODE = 0x01;

    public static final DocumentDictionary DICTIONARY = DocumentDictionaryBuilder.create(CODE)
            .context(ContextDictionary.INSTANCE)
            .build();
}
