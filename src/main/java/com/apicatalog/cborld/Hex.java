package com.apicatalog.cborld;

public class Hex {

    public static final String toString(byte[] array) {
	final StringBuilder builder = new StringBuilder(array.length * 5 - 1 + 2).append('[');

	for (int i = 0; i < array.length; i++) {
	    if (i > 0) {
		builder.append(',');
	    }
	    builder.append(toString(array[i]));
	}

	return builder.append(']').toString();
    }

    public static final String toString(byte value) {
	return String.format("0x%02X", value);
    }
    
}
