package com.legadi.jurl.executor;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.ResponseEntry;

public interface RequestExecutor<T extends RequestEntry, R extends ResponseEntry> {

    boolean accepts(RequestEntry request);

    R executeRequest(Settings settings, T request) throws RequestException;
}
