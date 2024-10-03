package com.legadi.cli.jurl.embedded.executor;

import static com.legadi.cli.jurl.embedded.util.ObjectName.ASSERTIONS_RESULT;

import java.util.Optional;
import java.util.UUID;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.util.RequestCatcher;
import com.legadi.cli.jurl.executor.http.HTTPResponseProcessor;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class HTTPResponseTestProcessor extends HTTPResponseProcessor {

    private final UUID identifier;
    private final RequestCatcher requestCatcher;

    public HTTPResponseTestProcessor(UUID identifier, RequestCatcher requestCatcher) {
        this.identifier = identifier;
        this.requestCatcher = requestCatcher;
    }

    @Override
    public Optional<AssertionResult> processResponse(Settings settings,
            HTTPRequestEntry request, HTTPResponseEntry response) {
        return requestCatcher.add(identifier, ASSERTIONS_RESULT,
            super.processResponse(settings, request, response));
    }
}
