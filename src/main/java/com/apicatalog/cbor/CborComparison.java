package com.apicatalog.cbor;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;

public class CborComparison {

    public static final boolean equals(byte[] value1, byte[] value2) throws CborException {
	
	if (value1 == null || value2 == null) {
	    return value1 == value2;
	}

	if (value1.length != value2.length) {
	    return false;
	}
	
	final List<DataItem> decoded1 = CborDecoder.decode(value1);
	final List<DataItem> decoded2 = CborDecoder.decode(value2);
	
	if (decoded1 == null || decoded2 == null) {
	    return decoded1 == decoded2;
	}
	
	return equals(decoded1, decoded2);
    }
    
    public static final boolean equals(Collection<DataItem> value1, Collection<DataItem> value2) {
	
	if (value1 == null || value2 == null) {
	    return value1 == value2;
	}

	if (value1.size() != value2.size()) {
	    return false;
	}

	Iterator<DataItem> it1 = value1.iterator();
	Iterator<DataItem> it2 = value2.iterator();
	
	while (it1.hasNext()) {
	    if (!equals(it1.next(), it2.next())) {
		return false;
	    }
	}
	return true;
    }
    
    public static final boolean equals(DataItem value1, DataItem value2) {
	
	if (value1 == null || value2 == null) {
	    return value1 == value2;
	}

	if (value1.getMajorType() != value2.getMajorType()) {
	    return false;
	}
	
	switch (value1.getMajorType()) {
	case ARRAY:
	    return equals(((Array)value1).getDataItems(), ((Array)value2).getDataItems());
	    
	case MAP:
	    
	    
	default:
	    return value1.equals(value2);
	}
    }
    
    public static final boolean equals(Map value1, Map value2) { 

	if (value1 == null || value2 == null) {
	    return value1 == value2;
	}

	if (value1.getKeys().size() != value2.getKeys().size()) {
	    return false;
	}
	
	final Iterator<DataItem> keys = value1.getKeys().iterator();

	while (keys.hasNext()) {
	    final DataItem key = keys.next();
	    if (!equals(value1.get(key), value2.get(key))) {
		return false;
	    }
	}
	return true;
    }
}
