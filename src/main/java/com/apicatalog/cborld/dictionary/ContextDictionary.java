package com.apicatalog.cborld.dictionary;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.hex.Hex;

/**
 * A registry of well-know term values as defined by the specification.
 * 
 * @see <a href="https://digitalbazaar.github.io/cbor-ld-spec/#term-codec-registry">Term Codeec Registry</a>
 *
 */
public class ContextDictionary implements Dictionary {

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
    public BigInteger getCode(String term) {
	Integer code = CODEC_INDEX.get(term.hashCode());

	if (code != null) {
	    return BigInteger.valueOf(code);
	}

	return null;
    }

    @Override
    public String getValue(BigInteger code) {
	
	final int value = code.intValueExact();
	
	if (value < OFFSET) {
	    throw new IllegalArgumentException(
		    		"Invalid code " + Hex.toString(code.toByteArray())  + "."
		    		+ "Available range " 
		    	        	+ Hex.toString(OFFSET) 
		    	        	+ "-" 
		    	        	+ Hex.toString((byte)(OFFSET + TERMS.length))
		    	        	+ "."
		    	        );
	}
	return TERMS[value - OFFSET];
    }
    
    public String getTerm(Integer code) {
	return TERMS[code - OFFSET];
    }
}
