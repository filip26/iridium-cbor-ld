package com.apicatalog.cborld;

/**
 * A registry of well-know term values as defined by the specification.
 * 
 * @see <a href="https://digitalbazaar.github.io/cbor-ld-spec/#term-codec-registry">Term Codeec Registry</a>
 *
 */
public class DefaultDictionary implements Dictionary {

    private static byte OFFSET = 0x10;
    
    private static String[] TERMS = new String[] {
	    "https://www.w3.org/ns/activitystreams",
	    "https://www.w3.org/2018/credentials/v1",
	    "https://www.w3.org/ns/did/v1",
	    "https://w3id.org/security/suites/ed25519-2018/v1",
	    "https://w3id.org/security/suites/ed25519-2020/v1",
	    "https://w3id.org/cit/v1",
	    "https://w3id.org/age/v1",
	    "https://w3id.org/security/suites/x25519-2020/v1",
	    "https://w3id.org/veres-one/v1",
	    "https://w3id.org/webkms/v1",
	    "https://w3id.org/zcap/v1",
	    "https://w3id.org/security/suites/hmac-2019/v1",
	    "https://w3id.org/security/suites/aes-2019/v1",
	    "https://w3id.org/vaccination/v1",
	    "https://w3id.org/vc-revocation-list-2020/v1",
	    "https://w3id.org/dcc/v1",
	    "https://w3id.org/vc/status-list/v1",
    };
    
    @Override
    public byte getCode(String term) {
	
	int index = indexOf(term);
	
	if (index == -1) {
	    //TODO
	}

	return (byte)(OFFSET + index);
    }

    @Override
    public String getTerm(byte code) {
	if (code < OFFSET) {
	    throw new IllegalArgumentException("Invalid code [" + Integer.toHexString(code)  + "]. Codes of range 0x00-0x0f are reserved.");
	}

	return TERMS[code - OFFSET];
    }

    private static int indexOf(final String term) {
	for (int i=0; i < TERMS.length; i++) {
	    if (TERMS[i].equals(term)) {
		return i;
	    }
	}
	return -1;
    }
}
