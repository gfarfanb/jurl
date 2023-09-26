package com.legadi.jurl.embedded.executor;

import java.util.UUID;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.util.RequestCatcher;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.executor.http.HTTPRequestExecutor;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class HTTPRequestTestExecutor extends HTTPRequestExecutor {

    private final UUID identifier;
    private final RequestCatcher requestCatcher;

    public HTTPRequestTestExecutor(UUID identifier, RequestCatcher requestCatcher) {
        this.identifier = identifier;
        this.requestCatcher = requestCatcher;
    }

    @Override
    public HTTPResponseEntry executeRequest(Settings settings, HTTPRequestEntry request) throws RequestException {
        requestCatcher.add(new TypeToken<Settings>() {}, identifier, settings);
        requestCatcher.add(new TypeToken<HTTPRequestEntry>() {}, identifier, request);
        return requestCatcher.add(new TypeToken<HTTPResponseEntry>() {},
            identifier, super.executeRequest(settings, request));
    }
}
