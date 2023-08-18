package com.legadi.jurl.executor;

import com.legadi.jurl.common.ExecutionTag;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.ResponseEntry;

public interface RequestExecutor<T extends RequestEntry, R extends ResponseEntry> {

    boolean accepts(RequestEntry request);

    R executeRequest(ExecutionTag executionTag, T request) throws RequestException;
}
