package com.apicatalog.cborld;

import java.util.Arrays;
import java.util.Objects;

public enum CborLdVersion {

    V1(CborLd.VERSION_1_BYTES),

    // legacy
    V06(new byte[] { CborLd.VERSION_06_BYTE }),
    V05(new byte[] { CborLd.VERSION_05_BYTE });

    private final byte[] bytes;

    CborLdVersion(byte[] bytes) {
        this.bytes = bytes;
    }

    public byte[] bytes() {
        return bytes;
    }

    /**
     * Identifies the CborLdVersion from a given byte array and offset.
     *
     * @param bytes  The input byte array
     * @param offset The offset to start checking from
     * @return Matching CborLdVersion or {@code null} if not found
     */
    public static CborLdVersion of(byte[] bytes, int offset) {
        Objects.requireNonNull(bytes);

        for (var version : values()) {
            var versionBytes = version.bytes;
            int end = offset + versionBytes.length;

            if (end <= bytes.length && Arrays.mismatch(versionBytes, 0, versionBytes.length, bytes, offset, end) == -1) {
                return version;
            }
        }
        return null;
    }
}
