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

    default void mergeAPIDefinition(Settings settings, RequestEntry<? extends MockEntry> api, RequestEntry<? extends MockEntry> request) {
        mergeAPI(settings, cast(api), cast(request));
    }

    void mergeAPI(Settings settings, T api, T request);

    default void overrideWithFile(Settings settings, RequestEntry<? extends MockEntry> request, String filename) {
        overrideRequest(settings, cast(request), filename);
    }

    void overrideRequest(Settings settings, T request, String filename);
}
