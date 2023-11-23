package com.legadi.jurl.executor;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.RequestInput;
import com.legadi.jurl.model.ResponseEntry;

public interface RequestModifier<T extends RequestEntry<? extends MockEntry>, R extends ResponseEntry>
        extends RequestType<T, R> {

    default Pair<String, RequestInput<?>> appendAuthenticationIfExists(Settings settings, RequestInput<?> requestInput) {
        return appendAuthenticationDefinition(settings, cast(requestInput));
    }

    Pair<String, RequestInput<?>> appendAuthenticationDefinition(Settings settings, RequestInput<T> requestInput);

    default void mergeHeader(RequestEntry<? extends MockEntry> api,
            RequestEntry<? extends MockEntry> request) {
        mergeRequestHeader(cast(api), cast(request));
    }

    void mergeRequestHeader(T api, T request);

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
