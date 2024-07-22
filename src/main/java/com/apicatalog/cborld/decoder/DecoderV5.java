package com.apicatalog.cborld.decoder;

import java.io.ByteArrayInputStream;
import java.util.List;

import com.apicatalog.cborld.context.ContextError;
import com.apicatalog.cborld.decoder.DecoderError.Code;
import com.apicatalog.cborld.loader.StaticContextLoader;
import com.apicatalog.cborld.mapper.Mapping;
import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.HttpLoader;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;
import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonValue;

public class DecoderV5 extends Decoder {

    protected final boolean compressed;

    protected DecoderV5(byte[] encoded, boolean compressed) {
        super(encoded);
        this.compressed = compressed;
    }

    /**
     * Decode CBOR-LD document as JSON-LD document.
     * 
     * @return a decoded CBOR-LD document
     *
     * @throws ContextError
     * @throws DecoderError
     */
    public JsonValue decode() throws ContextError, DecoderError {

        if (loader == null) {
            loader = new HttpLoader(DefaultHttpClient.defaultInstance());
            ((HttpLoader) loader).fallbackContentType(MediaType.JSON);
        }

        if (bundledContexts) {
            loader = new StaticContextLoader(loader);
        }

        if (compressed) {
            return decodeCompressed();
        }
        return decodeUncompressed();
    }

    final JsonValue decodeCompressed() throws DecoderError, ContextError {

        try {
            final ByteArrayInputStream bais = new ByteArrayInputStream(encoded);
            final List<DataItem> dataItems = new CborDecoder(bais).decode();

            // nothing do de-compress
            if (dataItems.isEmpty()) {
                return null;
            }

            // decode as an array of objects
            if (dataItems.size() > 1) {

                final JsonArrayBuilder builder = Json.createArrayBuilder();

                for (final DataItem item : dataItems) {

                    builder.add(decodeCompressed(item));
                }

                return builder.build();
            }

            return decodeCompressed(dataItems.iterator().next());

        } catch (final CborException e) {
            throw new DecoderError(Code.InvalidDocument, e);
        }
    }

    final JsonValue decodeCompressed(final DataItem data) throws DecoderError, ContextError {

        final Mapping mapping = provider.getDecoderMapping(data, base, loader, this);

        index = mapping.dictionary();

        return decodeData(data, null, mapping.typeMap());
    }
    

    final JsonValue decodeUncompressed() throws DecoderError {
        throw new DecoderError(Code.InvalidDocument, "Unsupported document compression algorithm");
    }

}
