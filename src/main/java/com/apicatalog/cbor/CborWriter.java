package com.apicatalog.cbor;

import java.io.Writer;
import java.util.List;

import co.nstant.in.cbor.model.DataItem;

public class CborWriter {

    final Writer writer;
    
    public CborWriter(Writer writer) {
        this.writer = writer;
    }
    
    public void write(List<DataItem> value) {
        
    }
    
}
