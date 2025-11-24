package com.apicatalog.cborld.debug;

import java.net.URI;

import com.apicatalog.cborld.encoder.DefaultEncoder;
import com.apicatalog.cborld.encoder.Encoder;
import com.apicatalog.cborld.encoder.EncoderConfig;
import com.apicatalog.cborld.encoder.EncoderException;
import com.apicatalog.cborld.encoder.EncoderMappingProvider;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIO;

/**
 * A debug-oriented implementation of the CBOR-LD encoder.
 *
 * <p>
 * The {@code DebugEncoder} allows introspection of the encoding process,
 * including version and dictionary resolution, mapping extraction, and error
 * reporting. This is particularly useful for diagnostics, testing, and
 * verifying encoder behavior.
 * </p>
 *
 * <p>
 * It extends {@link Debug} and stores metadata during encoding, such as the
 * generated CBOR-LD output, used dictionary, and any exception thrown.
 * </p>
 */
public class DebugEncoder extends Debug {

    /** The configuration used to build the encoder. */
    final EncoderConfig config;

    /**
     * Creates a new {@code DebugEncoder} instance.
     *
     * @param config the encoder configuration
     * @param loader the document loader for resolving external contexts
     * @param base   the base URI for relative resolution
     */
    public DebugEncoder(EncoderConfig config, DocumentLoader loader, URI base) {
        super(loader, base);
        this.config = config;
    }

    /**
     * Encodes a JSON-LD document and captures debug metadata.
     * <p>
     * If encoding fails, the error can be retrieved using {@link #error()}.
     *
     * @param object the JSON-LD input to encode
     */
    public void encode(Object object, TreeAdapter adapter) {
        try {

            var debug = new DefaultEncoder(
                    config,
                    new DebugMapping(config.encoderMapping(), this),
                    loader,
                    base);

            version = config.version();
            dictionary = config.dictionary();
            encoded = debug.encode(object, adapter);

        } catch (EncoderException e) {
            this.error = e;
        }
    }
    
    public void encode(TreeIO node) {
        encode(node.node(), node.adapter());
    }

    /**
     * A wrapper around {@link EncoderMappingProvider} that injects debug metadata.
     * Captures the encoder mapping used during encoding.
     *
     * @param provider the original mapping provider
     * @param debug    the debug container to populate
     */
    private static record DebugMapping(
            EncoderMappingProvider provider,
            Debug debug) implements EncoderMappingProvider {

        @Override
        public Mapping getEncoderMapping(Object document, TreeAdapter adapter, Encoder encoder) throws EncoderException {
            debug.mapping = provider.getEncoderMapping(document, adapter, encoder);
            return debug.mapping;
        }
    }
}
