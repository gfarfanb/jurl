package com.legadi.cli.jurl.executor;

import java.util.List;
import java.util.Optional;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.AuthenticationRequest;
import com.legadi.cli.jurl.model.MockEntry;
import com.legadi.cli.jurl.model.RequestEntry;
import com.legadi.cli.jurl.model.RequestInput;
import com.legadi.cli.jurl.model.ResponseEntry;
import com.legadi.cli.jurl.options.OptionsReader.OptionEntry;

public interface RequestModifier<T extends RequestEntry<? extends MockEntry>, R extends ResponseEntry>
        extends RequestType<T, R> {

    default Optional<AuthenticationRequest<T>> getAuthenticationIfExists(String requestName,
            String requestInputPath, RequestInput<?> requestInput, Settings settings,
            List<OptionEntry> options) {
        return getAuthenticationDefinition(requestName, requestInputPath, cast(requestInput), settings, options);
    }

    Optional<AuthenticationRequest<T>> getAuthenticationDefinition(String requestName,
        String requestInputPath, RequestInput<T> requestInput, Settings settings, List<OptionEntry> options);

    default void mergeHeader(RequestEntry<? extends MockEntry> api,
            RequestEntry<? extends MockEntry> request) {
        mergeRequestHeader(cast(api), cast(request));
    }

    void mergeRequestHeader(T api, T request);

    default void mergeAPI(Settings settings, RequestEntry<? extends MockEntry> api,
            RequestEntry<? extends MockEntry> request) {
        mergeAPIDefinition(settings, cast(api), cast(request));
    }

    void mergeAPIDefinition(Settings settings, T api, T request);

    default void mergeBody(Settings settings, String requestPath,
            RequestEntry<? extends MockEntry> request) {
        mergeBodyFileWithBodyContent(settings, requestPath, cast(request));
    }

    void mergeBodyFileWithBodyContent(Settings settings, String requestPath, T request);

    default void overrideRequest(Settings settings, RequestEntry<? extends MockEntry> request,
            String filename) {
        overrideRequestWithFile(settings, cast(request), filename);
    }

    void overrideRequestWithFile(Settings settings, T request, String filename);
}
