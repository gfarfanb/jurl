package com.legadi.jurl.executor;

import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.RequestEntry;

public interface RequestExecutor<T extends RequestEntry> {

    boolean accepts(RequestEntry request);

    void executeRequest(T request) throws RequestException;
}
