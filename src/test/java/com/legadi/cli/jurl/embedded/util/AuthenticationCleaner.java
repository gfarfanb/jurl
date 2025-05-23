package com.legadi.cli.jurl.embedded.util;

import static com.legadi.cli.jurl.common.CommonUtils.toGeneratedParam;
import static com.legadi.cli.jurl.common.JsonUtils.removeJsonProperties;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findAll;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.executor.HeaderAuthenticator;
import com.legadi.cli.jurl.executor.http.HTTPTokenHeaderAuthenticator;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;

public class AuthenticationCleaner {

    private AuthenticationCleaner() {}

    public static void cleanup(RequestCatcher requestCatcher, UUID correlationId) {
        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);
        Set<String> clientIds = requestCatcher.<HTTPRequestEntry>getAll(correlationId, REQUEST)
            .stream()
            .map(HTTPRequestEntry::getAuthEntries)
            .map(Map::values)
            .flatMap(Collection::stream)
            .filter(auth -> HTTPTokenAuthEntry.class.isAssignableFrom(auth.getClass()))
            .map(auth -> (HTTPTokenAuthEntry) auth)
            .map(HTTPTokenAuthEntry::getClientId)
            .collect(Collectors.toSet());
        List<String> timeUnits = findAll(HeaderAuthenticator.class, settings.getRequestType())
            .stream()
            .filter(auth -> HTTPTokenHeaderAuthenticator.class.isAssignableFrom(auth.getClass()))
            .map(auth -> (HTTPTokenHeaderAuthenticator) auth)
            .map(auth -> auth.getExpiresInTimeUnit(settings))
            .collect(Collectors.toList());

        for(String clientId : clientIds) {
            List<String> params = timeUnits
                .stream()
                .map(unit -> toGeneratedParam(settings.getRequestType(), clientId, "expires-in." + unit))
                .collect(Collectors.toList());

            params.add(toGeneratedParam(settings.getRequestType(), clientId, "expiration-millis"));
            params.add(toGeneratedParam(settings.getRequestType(), clientId, "access-token"));
            params.add(toGeneratedParam(settings.getRequestType(), clientId, "token-type"));
            params.add(toGeneratedParam(settings.getRequestType(), clientId, "expiration-date"));

            removeJsonProperties(
                settings.getOverrideFilePath(),
                params.toArray(new String[params.size()]));
        }
}
}
