package com.apicatalog.cborld;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.cborld.registry.DocumentDictionaryBuilder;

public class UtopiaBarcodeExtended {

    public static final byte DICTIONARY_CODE = 101;

    public static final DocumentDictionary DICTIONARY = DocumentDictionaryBuilder
            .create(DICTIONARY_CODE)
            .context("https://www.w3.org/ns/credentials/v2", 1)
            .context("https://w3id.org/vc-barcodes/v1", 2)
            .type("https://w3id.org/security#cryptosuiteString",
                    Dictionary.newBuilder()
                            .set("ecdsa-rdfc-2019", 1)
                            .set("ecdsa-sd-2023", 2)
                            .set("eddsa-rdfc-2022", 3)
                            .set("ecdsa-xi-2023", 4))
            .uri("did:key:zDnaeWjKfs1ob9QcgasjYSPEMkwq31hmvSAWPVAgnrt1e9GKj", 1)
            .uri("did:key:zDnaeWjKfs1ob9QcgasjYSPEMkwq31hmvSAWPVAgnrt1e9GKj#zDnaeWjKfs1ob9QcgasjYSPEMkwq31hmvSAWPVAgnrt1e9GKj", 2)            
            .uri("https://sandbox.platform.veres.dev/statuses/z19rJ4oGrbFCqf3cNTVDHSbNd/status-lists", 3)
            .uri("did:key:zDnaeZSD9XcuULaS8qmgDUa6TMg2QjF9xABnZK42awDH3BEzj", 4)
            .uri("did:key:zDnaeZSD9XcuULaS8qmgDUa6TMg2QjF9xABnZK42awDH3BEzj#zDnaeZSD9XcuULaS8qmgDUa6TMg2QjF9xABnZK42awDH3BEzj", 5)
            .build();
}