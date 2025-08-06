package com.apicatalog.cborld;

import java.util.Arrays;
import java.util.Objects;

/**
 * Supported CBOR-LD format versions.
 * <p>
 * Each version is represented by a unique byte signature that can be used to
 * identify the encoded format.
 */
public enum CborLdVersion {

    /** CBOR-LD version 1.0 */
    V1(CborLd.VERSION_1_BYTES),

    /** Legacy version 0.6 */
    V06(new byte[] { CborLd.VERSION_06_BYTE }),

    /** Legacy version 0.5 */
    V05(new byte[] { CborLd.VERSION_05_BYTE });

    private final byte[] bytes;

    CborLdVersion(byte[] bytes) {
        this.bytes = bytes;
    }

    /**
     * Returns the version signature bytes associated with this CBOR-LD version.
     *
     * @return the version identifier bytes
     */
    public byte[] bytes() {
        return bytes;
    }

    /**
     * Determines the {@link CborLdVersion} based on the content of a byte array
     * starting at the given offset.
     * <p>
     * This method compares the input bytes against all known version identifiers.
     *
     * @param bytes  the byte array containing the encoded CBOR-LD data
     * @param offset the starting offset to compare the version bytes
     * @return the matching {@code CborLdVersion}, or {@code null} if no match is
     *         found
     * @throws NullPointerException if {@code bytes} is {@code null}
     */
    public static CborLdVersion of(byte[] bytes, int offset) {
        Objects.requireNonNull(bytes);

        for (var version : values()) {
            var versionBytes = version.bytes;
            int end = offset + versionBytes.length;

            if (end <= bytes.length &&
                    Arrays.mismatch(versionBytes, 0, versionBytes.length, bytes, offset, end) == -1) {
                return version;
            }
        }

        return null;
    }
}
