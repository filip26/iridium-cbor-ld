package com.apicatalog.cborld;

import com.apicatalog.cborld.mapping.TermMap;
import com.apicatalog.cborld.registry.DocumentDictionary;

public class UtopiaBarcode {

    public static final byte DICTIONARY_CODE = 100;

    public static final DocumentDictionary DICTIONARY = DocumentDictionary.newBuilder(DICTIONARY_CODE)
            .context("https://www.w3.org/ns/credentials/v2", 32768)
            .context("https://w3id.org/vc-barcodes/v1", 32769)
            .context("https://w3id.org/utopia/v2", 32770)
            .type("https://w3id.org/security#cryptosuiteString",
                    TermMap.newBuilder()
                            .set("ecdsa-rdfc-2019", 1)
                            .set("ecdsa-sd-2023", 2)
                            .set("eddsa-rdfc-2022", 3)
                            .set("ecdsa-xi-2023", 4))
            .build();
}