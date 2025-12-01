package com.apicatalog.cbor;

import java.io.IOException;
import java.io.Writer;
import java.util.List;

import com.apicatalog.cborld.hex.Hex;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;

public class CborWriter {

    final Writer writer;

    public CborWriter(Writer writer) {
        this.writer = writer;
    }

    public void write(List<DataItem> value) throws IOException {

        writer.write('[');

        boolean first = true;

        for (final DataItem item : value) {
            if (!first) {
                writer.write(", ");
            }
            write(item);
            first = false;
        }

        writer.write(']');
    }

    public void write(DataItem value) throws IOException {

        switch (value) {
        case Array array:
            write(array.getDataItems());
            break;

        case Map map:
            write(map);
            break;

        case ByteString string:
            write(string);
            break;

        default:
            writer.write(value.toString());
        }
    }

    protected void write(Map value) throws IOException {

        writer.write('{');

        boolean first = true;

        for (final DataItem key : value.getKeys()) {
            if (!first) {
                writer.write(',');
            }
            writer.write(' ');
            writer.write(key.toString());
            writer.write(':');
            writer.write(' ');
            write(value.get(key));
            first = false;
        }
        writer.write(' ');
        writer.write('}');
    }

    protected void write(ByteString value) throws IOException {
        writer.write(Hex.toString(value.getBytes()));
    }
}
