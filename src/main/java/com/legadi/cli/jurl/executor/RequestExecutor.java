package com.legadi.cli.jurl.executor;

import java.util.Optional;

import com.google.gson.reflect.TypeToken;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.RequestException;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.MockEntry;
import com.legadi.cli.jurl.model.RequestEntry;
import com.legadi.cli.jurl.model.ResponseEntry;

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
