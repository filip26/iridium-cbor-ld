package com.apicatalog.cborld.decoder;

import java.net.URI;
import java.util.Collection;
import java.util.Map;

import com.apicatalog.cborld.CborLd.Version;
import com.apicatalog.cborld.decoder.DecoderException.DecoderError;
import com.apicatalog.jsonld.loader.DocumentLoader;

class MultiDecoder implements Decoder {

    protected final Map<Version, Decoder> decoders;
    protected final DocumentLoader loader;
    protected final URI base;
    
    protected MultiDecoder(Map<Version, Decoder> decoders, DocumentLoader loader, URI base) {
        this.decoders = decoders;
        this.loader = loader;
        this.base = base;
    }

    @Override
    public Object decode(byte[] encoded) throws DecoderException {
        return decode(Decoder.assertCborLd(encoded), encoded);
    }

    @Override
    public Object decode(Version version, byte[] encoded) throws DecoderException {
        final Decoder decoder = decoders.get(version);

        if (decoder == null) {
            throw new DecoderException(DecoderError.UNSUPPORTED, "The decoder is not configured to support version " + version + " but " + decoders.keySet() +  ".");
        }

        return decoder.decode(version, encoded);
    }

    public Collection<Version> versions() {
        return decoders.keySet();
    }
    
    public Collection<Decoder> decoders() {
        return decoders.values();
    }

    @Override
    public DecoderConfig config() {
        throw new UnsupportedOperationException();
    }

    @Override
    public URI base() {
        return base;
    }

    @Override
    public DocumentLoader loader() {
        return loader;
    }
}
