package com.legadi.cli.jurl.embedded.executor;

import static com.legadi.cli.jurl.embedded.util.ObjectName.CONDITIONS_RESULT;
import static com.legadi.cli.jurl.embedded.util.ObjectName.EXECUTOR_EXECUTED;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST_INPUT_PATH;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST_WITH_EXCEPTION;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST_WITH_EXCEPTION_THROW;
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

public class HTTPRequestDummyExecutor extends HTTPRequestExecutor {

    private final UUID identifier;
    private final RequestCatcher requestCatcher;

    public HTTPRequestDummyExecutor(UUID identifier, RequestCatcher requestCatcher) {
        this.identifier = identifier;
        this.requestCatcher = requestCatcher;
    }

    @Override
    public Optional<AssertionResult> acceptsConditions(Settings settings, HTTPRequestEntry request) {
        return requestCatcher.getLast(identifier, CONDITIONS_RESULT);
    }

    @Override
    public HTTPResponseEntry executeRequest(Settings settings, String requestInputPath,
            HTTPRequestEntry request) throws RequestException {
        requestCatcher.add(identifier, EXECUTOR_EXECUTED, true);

        requestCatcher.add(identifier, SETTINGS, settings);
        requestCatcher.add(identifier, REQUEST_INPUT_PATH, requestInputPath);
        requestCatcher.add(identifier, REQUEST, request);

        if(request.getName().equals(requestCatcher.getLast(identifier, REQUEST_WITH_EXCEPTION))) {
            throw requestCatcher.<RuntimeException>getLast(identifier, REQUEST_WITH_EXCEPTION_THROW);
        }

        return requestCatcher.getLast(identifier, RESPONSE);
    }
}
