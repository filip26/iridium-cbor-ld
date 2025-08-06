package com.apicatalog.cborld.debug;

import java.net.URI;
import java.util.Map;

import com.apicatalog.cborld.CborLdVersion;
import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.BaseDecoder;
import com.apicatalog.cborld.decoder.Decoder;
import com.apicatalog.cborld.decoder.DecoderBuilder;
import com.apicatalog.cborld.decoder.DecoderConfig;
import com.apicatalog.cborld.decoder.DecoderException;
import com.apicatalog.cborld.decoder.DecoderException.Code;
import com.apicatalog.cborld.mapping.DecoderMappingProvider;
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
    final Map<CborLdVersion, DecoderConfig> versions;

    /**
     * Constructs a new {@code DebugDecoder} instance.
     *
     * @param versions the set of supported {@link CborLdVersion}s and their
     *                 corresponding configurations
     * @param loader   the document loader used to resolve external contexts
     * @param base     the base URI to resolve relative IRIs
     */
    public DebugDecoder(Map<CborLdVersion, DecoderConfig> versions, DocumentLoader loader, URI base) {
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
            version = BaseDecoder.assertCborLd(encoded);

            var config = versions.get(version);

            if (config == null) {
                throw new DecoderException(Code.Unsupported, "The decoder is not configured to support version " + version + " but " + versions.keySet() + ".");
            }

            var debug = DecoderBuilder.newInstance(
                    config,
                    new DebugMapping(config.decoderMapping(), this),
                    loader,
                    base);

            decoded = debug.decode(encoded);

        } catch (ContextError | DecoderException e) {
            this.error = e;
        }
    }

    /**
     * Internal {@link DecoderMappingProvider} used to intercept dictionary and
     * mapping creation during decoding to populate the debug structure.
     */
    record DebugMapping(
            DecoderMappingProvider provider,
            Debug debug) implements DecoderMappingProvider {

        @Override
        public Mapping getDecoderMapping(DataItem document, DocumentDictionary dictionary, Decoder decoder) throws DecoderException, ContextError {
            debug.dictionary = dictionary;
            debug.mapping = provider.getDecoderMapping(document, dictionary, decoder);
            return debug.mapping;
        }
    }
}
