package com.apicatalog.cborld;

import java.util.Arrays;

public enum CborLdVersion {

    V10(CborLd.VERSION_10_BYTES),
    
    // legacy
    V06_COMPRESSED(new byte[] { CborLd.VERSION_06_BYTE, CborLd.COMPRESSED_BYTE }),
    V05_COMPRESSED(new byte[] { CborLd.VERSION_05_BYTE, CborLd.COMPRESSED_BYTE }),
    V06_UNCOMPRESSED(new byte[] { CborLd.VERSION_06_BYTE, CborLd.UNCOMPRESSED_BYTE }),
    V05_UNCOMPRESSED(new byte[] { CborLd.VERSION_05_BYTE, CborLd.UNCOMPRESSED_BYTE });

    private final byte[] bytes;

    CborLdVersion(byte[] bytes) {
        this.bytes = bytes;
    }

    public byte[] bytes() {
        return bytes;
    }

    public static CborLdVersion of(byte[] bytes) {
        for (CborLdVersion version : CborLdVersion.values()) {
            if (Arrays.equals(version.bytes, 0, version.bytes.length, bytes, 0, version.bytes.length)) {
                return version;
            }
        }
        return null;
    }

}
