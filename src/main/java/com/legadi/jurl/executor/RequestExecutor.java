package com.legadi.jurl.executor;

import java.util.Optional;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.ResponseEntry;

public interface RequestExecutor<T extends RequestEntry<? extends MockEntry>, R extends ResponseEntry>
        extends RequestType<T, R> {

    TypeToken<T> requestType();

    default Optional<AssertionResult> accepts(Settings settings, RequestEntry<? extends MockEntry> request) {
        return acceptsConditions(settings, cast(request));
    }

    Optional<AssertionResult> acceptsConditions(Settings settings, T request);

    default R execute(Settings settings, String requestPath, RequestEntry<? extends MockEntry> request) throws RequestException {
        return executeRequest(settings, requestPath, cast(request));
    }

    R executeRequest(Settings settings, String requestPath, T request) throws RequestException;

}
