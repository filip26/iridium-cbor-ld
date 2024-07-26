package com.apicatalog.cborld.dictionary;

import java.util.Map;

public interface Dictionary extends Iterable<Map.Entry<Integer, String>> {

    Integer getCode(String value);

    String getValue(Integer code);

}
