package com.apicatalog.cursor.cbor;

import co.nstant.in.cbor.model.DataItem;

record CborCursorState(
            DataItem data,
            Integer index,
            String key
            ) {

}
