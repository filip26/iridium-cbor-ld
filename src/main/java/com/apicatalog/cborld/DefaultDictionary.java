package com.apicatalog.cborld;

import java.util.HashMap;
import java.util.Map;

/**
 * A registry of well-know term values as defined by the specification.
 * 
 * @see <a href="https://digitalbazaar.github.io/cbor-ld-spec/#term-codec-registry">Term Codeec Registry</a>
 *
 */
public class DefaultDictionary implements Dictionary {

    private static final byte OFFSET = 0x10;
    
    private static final String[] TERMS = new String[] {
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
    
    private static final Map<Integer, Integer> CODEC_INDEX = new HashMap<>();

    static {
	for (int i = 0; i < TERMS.length; i++) {
	    CODEC_INDEX.put(TERMS[i].hashCode(), OFFSET + i);
	}
    }
    
    @Override
    public byte[] getCode(String term) {
	Integer code = CODEC_INDEX.get(term.hashCode());
	
	if (code != null) {
	    return new byte[] { (byte)(int)code };
	}

	return null;
    }

    @Override
    public String getTerm(byte[] code) {
	if (code.length != 1 || code[0] < OFFSET) {
	    throw new IllegalArgumentException(
		    		"Invalid code " + toHexString(code)  + "."
		    		+ "Available range " 
		    	        	+ toHexString(OFFSET) 
		    	        	+ "-" 
		    	        	+ toHexString((byte)(OFFSET + TERMS.length))
		    	        	+ "."
		    	        );
	}
	return TERMS[code[0] - OFFSET];
    }

    private static final String toHexString(byte[] array) {
	final StringBuilder builder = new StringBuilder(array.length * 5 - 1 + 2).append('[');

	for (int i = 0; i < array.length; i++) {
	    if (i > 0) {
		builder.append(',');
	    }
	    builder.append(toHexString(array[i]));
	}

	return builder.append(']').toString();
    }

    private static final String toHexString(byte value) {
	return String.format("0x%02X", value);
    }
}
