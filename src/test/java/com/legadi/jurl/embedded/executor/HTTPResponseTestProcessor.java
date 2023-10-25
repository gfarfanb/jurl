package com.legadi.jurl.embedded.executor;

import java.util.Optional;
import java.util.UUID;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.util.RequestCatcher;
import com.legadi.jurl.executor.http.HTTPResponseProcessor;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class HTTPResponseTestProcessor extends HTTPResponseProcessor {

    private final UUID identifier;
    private final RequestCatcher requestCatcher;

    public HTTPResponseTestProcessor(UUID identifier, RequestCatcher requestCatcher) {
        this.identifier = identifier;
        this.requestCatcher = requestCatcher;
    }

    @Override
    public Optional<AssertionResult> processResponse(Settings settings, HTTPRequestEntry request, HTTPResponseEntry response) {
        return requestCatcher.add(identifier, "assertion-result",
            super.processResponse(settings, request, response));
    }
}
