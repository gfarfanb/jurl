package com.legadi.jurl.executor;

import java.util.Map;
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
    default T castRequest(RequestEntry<? extends MockEntry> request) {
        return (T) request;
    }

    @SuppressWarnings("unchecked")
    default R castResponse(ResponseEntry response) {
        return (R) response;
    }

    default Optional<AssertionResult> accepts(Settings settings, RequestEntry<? extends MockEntry> request) {
        return acceptsConditions(settings, castRequest(request));
    }

    Optional<AssertionResult> acceptsConditions(Settings settings, T request);

    default R execute(Settings settings, String requestPath, RequestEntry<? extends MockEntry> request) throws RequestException {
        return executeRequest(settings, requestPath, castRequest(request));
    }

    R executeRequest(Settings settings, String requestPath, T request) throws RequestException;

    default void mergeAPI(Settings settings, RequestEntry<? extends MockEntry> api, RequestEntry<? extends MockEntry> request) {
        mergeAPIDefinition(settings, castRequest(api), castRequest(request));
    }

    void mergeAPIDefinition(Settings settings, T api, T request);

    default void mergeBody(Settings settings, String requestPath, RequestEntry<? extends MockEntry> request) {
        mergeBodyFileWithBodyContent(settings, requestPath, castRequest(request));
    }

    void mergeBodyFileWithBodyContent(Settings settings, String requestPath, T request);

    default void overrideRequest(Settings settings, RequestEntry<? extends MockEntry> request, String filename) {
        overrideRequestWithFile(settings, castRequest(request), filename);
    }

    void overrideRequestWithFile(Settings settings, T request, String filename);

    default Map<String, Object> getDetails(ResponseEntry response) {
        return getDetailsFromResponse(castResponse(response));
    }
    Map<String, Object> getDetailsFromResponse(R response);
}
