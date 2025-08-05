package com.apicatalog.cborld;

import java.util.Arrays;

public enum CborLdVersion {

    V1(CborLd.VERSION_10_BYTES),

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

    public static CborLdVersion of(byte[] bytes, int offset) {
        for (CborLdVersion version : CborLdVersion.values()) {
            if (((offset + version.bytes.length) < bytes.length)
                    && Arrays.equals(version.bytes, 0, version.bytes.length, bytes, offset, offset + version.bytes().length)) {
                return version;
            }
        }
        return null;
    }

}
