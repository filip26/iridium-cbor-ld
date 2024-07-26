package com.apicatalog.cborld.document;

import com.apicatalog.cborld.dictionary.Dictionary;
import com.apicatalog.cborld.dictionary.DictionaryBuilder;

public class CompressedDocument {

    public static final int CODE = 0x01;

    /**
     * A registry of well-know term values.
     */
    public static final Dictionary CONTEXTS = DictionaryBuilder.create()
            // 0x00 - 0x0F Reserved for future use.
            .set(0x10, "https://www.w3.org/ns/activitystreams")
            .set(0x11, "https://www.w3.org/2018/credentials/v1")
            .set(0x12, "https://www.w3.org/ns/did/v1")
            .set(0x13, "https://w3id.org/security/suites/ed25519-2018/v1")
            .set(0x14, "https://w3id.org/security/suites/ed25519-2020/v1")
            .set(0x15, "https://w3id.org/cit/v1")
            .set(0x16, "https://w3id.org/age/v1")
            .set(0x17, "https://w3id.org/security/suites/x25519-2020/v1")
            .set(0x18, "https://w3id.org/veres-one/v1")
            .set(0x19, "https://w3id.org/webkms/v1")
            .set(0x1A, "https://w3id.org/zcap/v1")
            .set(0x1B, "https://w3id.org/security/suites/hmac-2019/v1")
            .set(0x1C, "https://w3id.org/security/suites/aes-2019/v1")
            .set(0x1D, "https://w3id.org/vaccination/v1")
            .set(0x1E, "https://w3id.org/vc-revocation-list-2020/v1")
            .set(0x1F, "https://w3id.org/dcc/v1")
            .set(0x20, "https://w3id.org/vc/status-list/v1")
            .set(0x21, "https://www.w3.org/ns/credentials/v2")
            // 0x22 - 0x2F Available for use.
            .set(0x30, "https://w3id.org/security/data-integrity/v1")
            .set(0x31, "https://w3id.org/security/multikey/v1")
            // 0x32 Reserved for future use.
            .set(0x33, "https://w3id.org/security/data-integrity/v2")
            // 0x34 - 0x36 Reserved for future use.
            .build();

    public static final DocumentDictionary DICTIONARY = DocumentDictionaryBuilder.create(CODE)
            .context(CONTEXTS)
            .build();
}
