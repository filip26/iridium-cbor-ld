package com.apicatalog.cborld.decoder;

import java.util.Map;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderError.Code;

import jakarta.json.JsonValue;

public class MultiDecoder implements Decoder {

    protected final Map<CborLdVersion, Decoder> decoders;

    protected MultiDecoder(Map<CborLdVersion, Decoder> decoders) {
        this.decoders = decoders;
    }

    /**
     * Decode CBOR-LD document as JSON-LD document.
     * 
     * @param encoded an encoded CBOR-LD document
     * 
     * @return a decoded CBOR-LD document
     *
     * @throws ContextError
     * @throws DecoderError
     */
    @Override
    public JsonValue decode(byte[] encoded) throws ContextError, DecoderError {
        return decode(BaseDecoder.assertVersion(encoded), encoded);
    }

    @Override
    public JsonValue decode(CborLdVersion version, byte[] encoded) throws ContextError, DecoderError {
        final Decoder decoder = decoders.get(version);

        if (decoder == null) {
            throw new DecoderError(Code.Unsupported, "The decoder is not configured to support version " + version + ".");
        }

        return decoder.decode(version, encoded);
    }
}
