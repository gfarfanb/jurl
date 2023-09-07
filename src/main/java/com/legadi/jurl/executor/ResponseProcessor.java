package com.legadi.jurl.executor;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.ResponseEntry;

public interface ResponseProcessor<T extends RequestEntry, R extends ResponseEntry> {

    @SuppressWarnings("unchecked")
    default T cast(RequestEntry request) {
        return (T) request;
    }

    @SuppressWarnings("unchecked")
    default R cast(ResponseEntry response) {
        return (R) response;
    }

    default void process(Settings settings, RequestEntry request, ResponseEntry response, 
            long nanoElapsed) throws RequestException {
        processResponse(settings, cast(request), cast(response), nanoElapsed);
    }

    void processResponse(Settings settings, T request, R response, long nanoElapsed) throws RequestException;
}
