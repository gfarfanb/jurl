package com.legadi.cli.jurl.executor;

import java.util.List;
import java.util.function.BiConsumer;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.FlowEntry;
import com.legadi.cli.jurl.model.MockEntry;
import com.legadi.cli.jurl.model.RequestEntry;
import com.legadi.cli.jurl.model.RequestInput;
import com.legadi.cli.jurl.model.ResponseEntry;
import com.legadi.cli.jurl.options.OptionsReader.OptionEntry;

public interface RequestModifier<T extends RequestEntry<? extends MockEntry>, R extends ResponseEntry>
        extends RequestType<T, R> {

    default List<T> getAuthenticationIfExists(String requestName,
            RequestInput<?> requestInput, Settings settings, BiConsumer<Settings, List<OptionEntry>> optionsProcessor) {
        return getAuthenticationDefinition(requestName, cast(requestInput), settings, optionsProcessor);
    }

    List<T> getAuthenticationDefinition(String requestName,
        RequestInput<T> requestInput, Settings settings, BiConsumer<Settings, List<OptionEntry>> optionsProcessor);

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

    void expandFlow(Settings settings, FlowEntry flow);

    default void expandRequest(Settings settings, RequestEntry<? extends MockEntry> request) {
        expandRequestDefinition(settings, cast(request));
    }

    void expandRequestDefinition(Settings settings, T request);
}
