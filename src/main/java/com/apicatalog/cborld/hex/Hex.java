package com.apicatalog.cborld.hex;

public class Hex {

    public static final String toString(int value) {
        return toString(intToByteArray(value));
    }

    public static final String toString(byte[] array) {
        return toString(array, 4);
    }
    
    public static final String toString(byte[] array, int max) {
        final StringBuilder builder = new StringBuilder(array.length * 5 - 1 + 2).append('[');
    
        for (int i = 0; i < Math.min(max, array.length); i++) {
            if (i > 0) {
                builder.append(',');
            }
            builder.append(toString(array[i]));
        }
        
        if (max < array.length) {
            builder.append(", ... " + (array.length) + " bytes");
        }
    
        return builder.append(']').toString();
    }

    public static final String toString(byte value) {
        return String.format("0x%02X", value);
    }

    static final byte[] intToByteArray(int value) {
        return new byte[] {
            (byte)(value >>> 24),
            (byte)(value >>> 16),
            (byte)(value >>> 8),
            (byte)value
            };
    }
}
