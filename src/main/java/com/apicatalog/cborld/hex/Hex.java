package com.apicatalog.cborld.hex;

/**
 * Utility class for formatting integer and byte values as hexadecimal strings.
 *
 * <p>
 * Provides convenient methods to convert integers, longs, and byte arrays into
 * human-readable hex strings, often useful for debugging or logging binary data
 * such as CBOR.
 * </p>
 */
public class Hex {

    /**
     * Converts the given {@code int} value into a hexadecimal string
     * representation.
     *
     * @param value the integer value to convert
     * @return a hex string representation of the integer, e.g.,
     *         {@code [0x00,0x01,0x02,0x03]}
     */
    public static final String toString(int value) {
        return toString(intToByteArray(value));
    }

    /**
     * Converts the given byte array into a hexadecimal string representation,
     * displaying up to 4 bytes by default.
     *
     * @param array the byte array to convert
     * @return a hex string representation of the array
     */
    public static final String toString(byte[] array) {
        return toString(array, 4);
    }

    /**
     * Converts the given byte array into a hexadecimal string representation,
     * showing up to {@code max} elements. If the array is longer, the output is
     * truncated and annotated with its total length.
     *
     * @param array the byte array to convert
     * @param max   the maximum number of bytes to include in the output
     * @return a formatted hex string, e.g., {@code [0x00,0x1A,0xFF, ... 12 bytes]}
     */
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

    /**
     * Converts a single byte into a hexadecimal string representation.
     *
     * @param value the byte value
     * @return a hex string, e.g., {@code 0x1F}
     */
    public static final String toString(byte value) {
        return String.format("0x%02X", value);
    }

    /**
     * Converts a {@code long} value into a hexadecimal string representation.
     *
     * @param value the long value
     * @return a hex string, e.g., {@code 0x1FA3}
     */
    public static final String toString(long value) {
        return String.format("0x%02X", value);
    }

    /**
     * Converts an {@code int} value to a 4-byte big-endian array.
     *
     * @param value the integer to convert
     * @return a byte array of length 4 representing the integer
     */
    static final byte[] intToByteArray(int value) {
        return new byte[] {
                (byte) (value >>> 24),
                (byte) (value >>> 16),
                (byte) (value >>> 8),
                (byte) value
        };
    }

}
