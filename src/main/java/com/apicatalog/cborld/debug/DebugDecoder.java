package com.apicatalog.cborld.debug;

import java.net.URI;
import java.util.Map;

import com.apicatalog.cborld.CborLd.Version;
import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.decoder.DecoderBuilder;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.decoder.DecoderMappingProvider;
import com.apicatalog.cborld.decoder.DecoderException.DecoderError;
import com.apicatalog.cborld.mapping.Mapping;
import com.apicatalog.cborld.registry.DocumentDictionary;
import com.apicatalog.jsonld.loader.DocumentLoader;

import co.nstant.in.cbor.model.DataItem;

/**
 * A debug-oriented decoder implementation for CBOR-LD.
 *
 * <p>
 * The {@code DebugDecoder} provides detailed insights into the decoding
 * process, including version detection, dictionary mapping, term/type
 * resolution, and error tracking. It extends {@link Debug} and populates the
 * debug state with data extracted during decoding.
 * </p>
 *
 * <p>
 * This decoder is useful for testing, diagnostics, and debugging of CBOR-LD
 * encoded documents.
 * </p>
 */
public class DebugDecoder extends Debug {

    /** A map of supported CBOR-LD versions and their decoding configurations. */
    final Map<Version, DecoderConfig> versions;

    /**
     * Constructs a new {@code DebugDecoder} instance.
     *
     * @param versions the set of supported {@link Version}s and their
     *                 corresponding configurations
     * @param loader   the document loader used to resolve external contexts
     * @param base     the base URI to resolve relative URIs
     */
    public DebugDecoder(Map<Version, DecoderConfig> versions, DocumentLoader loader, URI base) {
        super(loader, base);
        this.versions = versions;
    }

    /**
     * Attempts to decode a CBOR-LD byte array and populate the debug state.
     * <p>
     * Sets the detected version, dictionary, term mappings, and decoded result. If
     * an error occurs, it will be recorded and accessible via {@link #error()}.
     *
     * @param encoded the CBOR-LD encoded document
     */
    public void decode(byte[] encoded) {
        try {
            version = Decoder.assertCborLd(encoded);

            var config = versions.get(version);

            if (config == null) {
                throw new DecoderException(DecoderError.UNSUPPORTED, "The decoder is not configured to support version " + version + " but " + versions.keySet() + ".");
            }

            var debug = DecoderBuilder.newDecoder(
                    config,
                    new DebugMapping(config.decoderMapping(), this),
                    loader,
                    base);

            decoded = debug.decode(encoded);

        } catch (DecoderException e) {
            this.error = e;
        }
    }

    /**
     * Internal {@link DecoderMappingProvider} used to intercept dictionary and
     * mapping creation during decoding to populate the debug structure.
     */
    private static record DebugMapping(
            DecoderMappingProvider provider,
            Debug debug) implements DecoderMappingProvider {

        @Override
        public Mapping getDecoderMapping(DataItem document, DocumentDictionary dictionary, Decoder decoder) throws DecoderException {
            debug.dictionary = dictionary;
            debug.mapping = provider.getDecoderMapping(document, dictionary, decoder);
            return debug.mapping;
        }
    }
}
