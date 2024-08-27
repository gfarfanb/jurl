package com.legadi.cli.jurl.embedded.executor;

import java.util.Optional;
import java.util.UUID;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.util.RequestCatcher;
import com.legadi.cli.jurl.executor.http.HTTPResponseProcessor;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class HTTPResponseDummyProcessor extends HTTPResponseProcessor {

    private final UUID identifier;
    private final RequestCatcher requestCatcher;

    public HTTPResponseDummyProcessor(UUID identifier, RequestCatcher requestCatcher) {
        this.identifier = identifier;
        this.requestCatcher = requestCatcher;
    }

    @Override
    public Optional<AssertionResult> processResponse(Settings settings,
            HTTPRequestEntry request, HTTPResponseEntry response) {
        requestCatcher.add(identifier, "processor-executed", true);

        return requestCatcher.getLast(identifier, "assertions-result");
    }
}
