package com.legadi.jurl.embedded.executor;

import java.util.Optional;
import java.util.UUID;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.util.RequestCatcher;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.executor.http.HTTPRequestExecutor;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class HTTPRequestDummyExecutor extends HTTPRequestExecutor {

    private final UUID identifier;
    private final RequestCatcher requestCatcher;

    public HTTPRequestDummyExecutor(UUID identifier, RequestCatcher requestCatcher) {
        this.identifier = identifier;
        this.requestCatcher = requestCatcher;
    }

    @Override
    public Optional<AssertionResult> acceptsConditions(Settings settings, HTTPRequestEntry request) {
        return requestCatcher.getLast(identifier, "conditions-result");
    }

    @Override
    public HTTPResponseEntry executeRequest(Settings settings, String requestInputPath,
            HTTPRequestEntry request) throws RequestException {
        requestCatcher.add(identifier, "executor-executed", true);

        requestCatcher.add(identifier, "settings", settings);
        requestCatcher.add(identifier, "request-input-path", requestInputPath);
        requestCatcher.add(identifier, "request", request);

        if(request.getName().equals(requestCatcher.getLast(identifier, "request-with-exception"))) {
            throw requestCatcher.<RuntimeException>getLast(identifier, "request-with-exception-throw");
        }

        return requestCatcher.getLast(identifier, "response");
    }
}
