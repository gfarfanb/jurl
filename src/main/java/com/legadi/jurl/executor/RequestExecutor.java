package com.legadi.jurl.executor;

import java.util.Optional;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.ResponseEntry;

public interface RequestExecutor<T extends RequestEntry<? extends MockEntry>, R extends ResponseEntry> {

    String type();

    TypeToken<T> requestType();

    @SuppressWarnings("unchecked")
    default T cast(RequestEntry<? extends MockEntry> request) {
        return (T) request;
    }

    default Optional<AssertionResult> accepts(Settings settings, RequestEntry<? extends MockEntry> request) {
        return acceptsConditions(settings, cast(request));
    }

    Optional<AssertionResult> acceptsConditions(Settings settings, T request);

    default R execute(Settings settings, String requestPath, RequestEntry<? extends MockEntry> request) throws RequestException {
        return executeRequest(settings, requestPath, cast(request));
    }

    R executeRequest(Settings settings, String requestPath, T request) throws RequestException;

    default void mergeAPI(Settings settings, RequestEntry<? extends MockEntry> api, RequestEntry<? extends MockEntry> request) {
        mergeAPIDefinition(settings, cast(api), cast(request));
    }

    void mergeAPIDefinition(Settings settings, T api, T request);

    default void mergeBody(Settings settings, String requestPath, RequestEntry<? extends MockEntry> request) {
        mergeBodyFileWithBodyContent(settings, requestPath, cast(request));
    }

    void mergeBodyFileWithBodyContent(Settings settings, String requestPath, T request);

    default void overrideRequest(Settings settings, RequestEntry<? extends MockEntry> request, String filename) {
        overrideRequestWithFile(settings, cast(request), filename);
    }

    void overrideRequestWithFile(Settings settings, T request, String filename);
}
