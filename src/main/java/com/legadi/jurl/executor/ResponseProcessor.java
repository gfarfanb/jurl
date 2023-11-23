package com.legadi.jurl.executor;

import java.util.Map;
import java.util.Optional;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.ResponseEntry;

public interface ResponseProcessor<T extends RequestEntry<? extends MockEntry>, R extends ResponseEntry>
        extends RequestType<T, R> {

    default Optional<AssertionResult> process(Settings settings, RequestEntry<? extends MockEntry> request,
            ResponseEntry response) throws RequestException {
        return processResponse(settings, cast(request), cast(response));
    }

    Optional<AssertionResult> processResponse(Settings settings, T request, R response) throws RequestException;

    default Map<String, Object> getDetails(ResponseEntry response) {
        return getDetailsFromResponse(cast(response));
    }

    Map<String, Object> getDetailsFromResponse(R response);
}
