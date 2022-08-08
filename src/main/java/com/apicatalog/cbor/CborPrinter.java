package com.apicatalog.cbor;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.List;

import com.apicatalog.cborld.hex.Hex;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;

public final class CborPrinter {

    final PrintWriter printer;
    
    public CborPrinter(final PrintWriter writer) {
        this.printer = writer;
    }
    
    public void print(final List<DataItem> value) throws IOException {
        printer.print('[');
        
        boolean first = true;

        for (final DataItem item : value) {
            if (!first) {
                printer.print(", ");
            }
            print(item);
            first = false;
        }
        
        printer.write(']');        
    }
    
    public void print(final DataItem value) throws IOException {
        
        switch (value.getMajorType()) {
        case ARRAY:
            print(((Array)value).getDataItems());
            break;
            
        case MAP:
            print((Map)value);
            break;
            
        case BYTE_STRING:
            print((ByteString)value);
            break;
            
        default:
            printer.write(value.toString());
        }
    }
    
    void print(final Map value) throws IOException {
        
        printer.write('{');
        
        boolean first = true;
        
        for (final DataItem key : value.getKeys()) {
            if (!first) {
                printer.write(',');
            }
            printer.print(' ');
            printer.print(key.toString());
            printer.print(':');
            printer.print(' ');
            print(value.get(key));
            first = false;
        }
        printer.write(' ');
        printer.write('}');
    }

    void print(final ByteString value) throws IOException {
        printer.write(Hex.toString(value.getBytes()));
    }
}
