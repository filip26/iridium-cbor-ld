package com.apicatalog.cborld.dictionary;

import java.util.Map;

import com.apicatalog.cborld.Hex;

public class ContextTermsDictionary implements Dictionary {

    
    private final Map<Integer, Integer> index;
    
    public ContextTermsDictionary(Map<Integer, Integer> index) {
	this.index = index;
    }
    
    @Override
    public byte[] getCode(String term) {
	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public String getTerm(byte[] value) {
	
	
	// TODO Auto-generated method stub
	return null;
    }

    private static int byteArrayToLeInt(byte[] byteArray) {
	
	if (byteArray == null || byteArray.length > Integer.BYTES) {
	    throw new IllegalArgumentException("Cannot convert byte array " + Hex.toString(byteArray) + " into an integer value. An integer is represented by " + Integer.BYTES + " bytes.");
	}

	int value = 0;
	
	for (int i = 0; i < byteArray.length; i++) {
	    value |= (byteArray[1] & 0xFF) << (Byte.SIZE * i);
	}
	
	return value;
    }

    private static byte[] leIntToByteArray(int value) {
	byte[] byteArray = new byte[Integer.BYTES];
	byteArray[3] = (byte) (value >> Byte.SIZE * 3);
	byteArray[2] = (byte) (value >> Byte.SIZE * 2);   
	byteArray[1] = (byte) (value >> Byte.SIZE);   
	byteArray[0] = (byte) value;
	return byteArray;
    }

    /*
     * 
  ['@context', 0],
  ['@type', 2],
  ['@id', 4],
  ['@value', 6],
  // alphabetized after `@context`, `@type`, `@id`, `@value`
  // IDs <= 24 represented with 1 byte, IDs > 24 use 2+ bytes
  ['@direction', 8],
  ['@graph', 10],
  ['@included', 12],
  ['@index', 14],
  ['@json', 16],
  ['@language', 18],
  ['@list', 20],
  ['@nest', 22],
  ['@reverse', 24],
  // TODO: remove these? these only appear in frames and contexts
  ['@base', 26],
  ['@container', 28],
  ['@default', 30],
  ['@embed', 32],
  ['@explicit', 34],
  ['@none', 36],
  ['@omitDefault', 38],
  ['@prefix', 40],
  ['@preserve', 42],
  ['@protected', 44],
  ['@requireAll', 46],
  ['@set', 48],
  ['@version', 50],
  ['@vocab', 52]
  export const FIRST_CUSTOM_TERM_ID = 100;
     */
}
