package com.legadi.cli.jurl.embedded.executor;

import static com.legadi.cli.jurl.embedded.util.ObjectName.CONDITIONS_RESULT;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST_INPUT_PATH;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.Optional;
import java.util.UUID;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.util.RequestCatcher;
import com.legadi.cli.jurl.exception.RequestException;
import com.legadi.cli.jurl.executor.http.HTTPRequestExecutor;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class HTTPRequestTestExecutor extends HTTPRequestExecutor {

    private final UUID identifier;
    private final RequestCatcher requestCatcher;

    public HTTPRequestTestExecutor(UUID identifier, RequestCatcher requestCatcher) {
        this.identifier = identifier;
        this.requestCatcher = requestCatcher;
    }

    @Override
    public Optional<AssertionResult> acceptsConditions(Settings settings, HTTPRequestEntry request) {
        Optional<AssertionResult> conditionsResult = super.acceptsConditions(settings, request);
        requestCatcher.add(identifier, CONDITIONS_RESULT, conditionsResult);
        return conditionsResult;
    }

    @Override
    public HTTPResponseEntry executeRequest(Settings settings, String requestInputPath,
            HTTPRequestEntry request) throws RequestException {
        requestCatcher.add(identifier, SETTINGS, settings);
        requestCatcher.add(identifier, REQUEST_INPUT_PATH, requestInputPath);
        requestCatcher.add(identifier, REQUEST, request);
        return requestCatcher.add(identifier, RESPONSE,
            super.executeRequest(settings, requestInputPath, request));
    }
}
