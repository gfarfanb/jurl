package com.legadi.cli.jurl.embedded.util;

import static com.legadi.cli.jurl.common.CommonUtils.toGeneratedParam;
import static com.legadi.cli.jurl.common.JsonUtils.removeJsonProperties;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;

public class AuthenticationCleaner {

    private AuthenticationCleaner() {}

    public static void cleanup(RequestCatcher requestCatcher, UUID correlationId) {
        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);
        Set<String> clientIds = requestCatcher.<HTTPRequestEntry>getAll(correlationId, REQUEST)
            .stream()
            .map(HTTPRequestEntry::getAuthEntries)
            .flatMap(List::stream)
            .filter(auth -> HTTPTokenAuthEntry.class.isAssignableFrom(auth.getClass()))
            .map(auth -> (HTTPTokenAuthEntry) auth)
            .map(HTTPTokenAuthEntry::getClientId)
            .collect(Collectors.toSet());

        for(String clientId : clientIds) {
            removeJsonProperties(settings.getOverrideFilePath(),
                toGeneratedParam(settings.getRequestType(), clientId, "expiration-millis"),
                toGeneratedParam(settings.getRequestType(), clientId, "access-token"),
                toGeneratedParam(settings.getRequestType(), clientId, "expires-in." + settings.getAuthBearerExpiresInTimeUnit()),
                toGeneratedParam(settings.getRequestType(), clientId, "expiration-date"));
        }
}
}
