package com.legadi.jurl.executor;

import java.util.Optional;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.AssertionResult;
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

    default Optional<AssertionResult> process(Settings settings, RequestEntry request, ResponseEntry response) throws RequestException {
        return processResponse(settings, cast(request), cast(response));
    }

    Optional<AssertionResult> processResponse(Settings settings, T request, R response) throws RequestException;
}
