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

    public static CborLdVersion of(byte[] bytes) {
        for (CborLdVersion version : CborLdVersion.values()) {
            if (Arrays.equals(version.bytes, 0, version.bytes.length, bytes, 0, Math.min(bytes.length, version.bytes.length))) {
                return version;
            }
        }
        return null;
    }

}
