package com.legadi.jurl.executor;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.ResponseEntry;

public interface RequestExecutor<T extends RequestEntry, R extends ResponseEntry> {

    boolean accepts(RequestEntry request);

    TypeToken<T> type();

    @SuppressWarnings("unchecked")
    default T cast(RequestEntry request) {
        return (T) request;
    }

    default R execute(Settings settings, RequestEntry request) throws RequestException {
        return executeRequest(settings, cast(request));
    }

    R executeRequest(Settings settings, T request) throws RequestException;
}
