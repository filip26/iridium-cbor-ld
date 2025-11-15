package com.apicatalog.cborld.decoder;

import java.net.URI;
import java.util.Collection;
import java.util.Map;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.decoder.DecoderException.Code;
import com.apicatalog.cborld.mapping.context.ContextMappingException;
import com.apicatalog.jsonld.loader.DocumentLoader;

class MultiDecoder implements Decoder {

    protected final Map<CborLdVersion, Decoder> decoders;
    protected final DocumentLoader loader;
    protected final URI base;
    
    protected MultiDecoder(Map<CborLdVersion, Decoder> decoders, DocumentLoader loader, URI base) {
        this.decoders = decoders;
        this.loader = loader;
        this.base = base;
    }

    @Override
    public Object decode(byte[] encoded) throws ContextMappingException, DecoderException {
        return decode(Decoder.assertCborLd(encoded), encoded);
    }

    @Override
    public Object decode(CborLdVersion version, byte[] encoded) throws ContextMappingException, DecoderException {
        final Decoder decoder = decoders.get(version);

        if (decoder == null) {
            throw new DecoderException(Code.Unsupported, "The decoder is not configured to support version " + version + " but " + decoders.keySet() +  ".");
        }

        return decoder.decode(version, encoded);
    }

    public Collection<CborLdVersion> versions() {
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
