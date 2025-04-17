package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.common.CommonUtils.getAllFields;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Logger;
import java.util.stream.Stream;

import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.executor.HeaderAuthenticator;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPBasicAuthEntry;

public class HTTPBasicHeaderAuthenticator implements HeaderAuthenticator<HTTPRequestEntry, HTTPBasicAuthEntry> {

    private static final Logger LOGGER = Logger.getLogger(HTTPBasicHeaderAuthenticator.class.getName());

    private static final Map<String, Field> BASIC_AUTH_FIELDS = getAllFields(HTTPBasicAuthEntry.class);

    @Override
    public String type() {
        return "http";
    }

    @Override
    public boolean requiresExecution() {
        return false;
    }

    @Override
    public Map<String, Field> getObjectFields() {
        return BASIC_AUTH_FIELDS;
    }

    @Override
    public String getParserElement() {
        return "basic";
    }

    @Override
    public HTTPBasicAuthEntry instanceAuthEntry(Settings settings) {
        return new HTTPBasicAuthEntry();
    }

    @Override
    public Optional<HTTPRequestEntry> createAuthRequest(Settings settings, HTTPRequestEntry api,
            HTTPRequestEntry request, Map<String, Object> defaults) {
        return Optional.empty();
    }

    @Override
    public void cleanupAuth(Settings settings, HTTPRequestEntry api, HTTPRequestEntry request) {
    }

    @Override
    public void mergeAuthEntry(HTTPRequestEntry api, HTTPRequestEntry request) {
        Optional<HTTPBasicAuthEntry> apiAuthEntry = getAuthEntry(api);
        Optional<HTTPBasicAuthEntry> requestAuthEntry = getAuthEntry(request);

        if(!apiAuthEntry.isPresent() || !requestAuthEntry.isPresent()) {
            return;
        }

        mergeAuthEntry(apiAuthEntry.get(), requestAuthEntry.get());
    }

    @Override
    public List<Pair<String, String>> getAuthHeaders(Settings settings, HTTPRequestEntry request) {
        Optional<HTTPBasicAuthEntry> authEntry = getAuthEntry(request);
        List<Pair<String, String>> headers = new ArrayList<>();

        if(!authEntry.isPresent()) {
            return headers;
        }

        LOGGER.fine("Adding token authorization - username=" + authEntry.get().getUsername()
            + " password=" + authEntry.get().getPassword());

        String username = authEntry.get().getUsername();
        String password = authEntry.get().getPassword();
        String basicDecoded = username + ":" + password;
        String basicValue = Base64.getEncoder().encodeToString(basicDecoded.getBytes());

        headers.add(new Pair<>("Authorization", "Basic " + basicValue));

        return headers;
    }

    private void mergeAuthEntry(HTTPBasicAuthEntry api, HTTPBasicAuthEntry request) {
        if(isBlank(request.getUsername())) {
            request.setUsername(api.getUsername());
        }
        if(isBlank(request.getPassword())) {
            request.setPassword(api.getPassword());
        }
    }

    private Optional<HTTPBasicAuthEntry> getAuthEntry(HTTPRequestEntry request) {
        return Optional.ofNullable(request)
            .map(HTTPRequestEntry::getAuthEntries)
            .map(List::stream)
            .orElse(Stream.empty())
            .filter(a -> HTTPBasicAuthEntry.class.isAssignableFrom(a.getClass()))
            .map(a -> (HTTPBasicAuthEntry) a)
            .findFirst();
    }
}
