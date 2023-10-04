package com.legadi.jurl.executor;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.ResponseEntry;

public interface RequestExecutor<T extends RequestEntry<? extends MockEntry>, R extends ResponseEntry> {

    boolean accepts(RequestEntry<? extends MockEntry> request);

    TypeToken<T> type();

    @SuppressWarnings("unchecked")
    default T cast(RequestEntry<? extends MockEntry> request) {
        return (T) request;
    }

    default R execute(Settings settings, RequestEntry<? extends MockEntry> request) throws RequestException {
        return executeRequest(settings, cast(request));
    }

    R executeRequest(Settings settings, T request) throws RequestException;

    default void mergeAPI(Settings settings, RequestEntry<? extends MockEntry> api, RequestEntry<? extends MockEntry> request) {
        mergeAPIDefinition(settings, cast(api), cast(request));
    }

    void mergeAPIDefinition(Settings settings, T api, T request);

    default void mergeBody(Settings settings, RequestEntry<? extends MockEntry> request) {
        mergeBodyFileWithBodyContent(settings, cast(request));
    }

    void mergeBodyFileWithBodyContent(Settings settings, T request);

    default void overrideRequest(Settings settings, RequestEntry<? extends MockEntry> request, String filename) {
        overrideRequestWithFile(settings, cast(request), filename);
    }

    void overrideRequestWithFile(Settings settings, T request, String filename);
}
