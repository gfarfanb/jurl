package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.common.CommonUtils.getAllFields;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.toGeneratedParam;
import static com.legadi.cli.jurl.common.JsonUtils.removeJsonProperties;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Logger;

import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.StringExpander;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.executor.HeaderAuthenticator;
import com.legadi.cli.jurl.model.AssertionEntry;
import com.legadi.cli.jurl.model.AssertionType;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;

public class HTTPTokenHeaderAuthenticator implements HeaderAuthenticator<HTTPRequestEntry, HTTPTokenAuthEntry> {

    private static final Logger LOGGER = Logger.getLogger(HTTPTokenHeaderAuthenticator.class.getName());

    private static final Map<String, Field> TOKEN_AUTH_FIELDS = getAllFields(HTTPTokenAuthEntry.class);

    @Override
    public String type() {
        return "http";
    }

    @Override
    public boolean requiresExecution() {
        return true;
    }

    @Override
    public Map<String, Field> getObjectFields() {
        return TOKEN_AUTH_FIELDS;
    }

    @Override
    public String getParserElement() {
        return "token";
    }

    @Override
    public HTTPTokenAuthEntry instanceAuthEntry(Settings settings) {
        HTTPTokenAuthEntry authEntry = new HTTPTokenAuthEntry();
        authEntry.setGrantType(settings.getAuthBearerGrantType());
        return authEntry;
    }

    @Override
    public Optional<HTTPRequestEntry> createAuthRequest(Settings settings,
            HTTPRequestEntry api, HTTPRequestEntry request, Map<String, Object> defaults) {
        Optional<HTTPTokenAuthEntry> apiAuthEntry = findAuthEntry(api);
        Optional<HTTPTokenAuthEntry> requestAuthEntry = findAuthEntry(request);

        if(!requestAuthEntry.isPresent()) {
            apiAuthEntry.ifPresent(auth -> request.getAuthEntries().put(auth.getParserElement(), auth));
        } else if(apiAuthEntry.isPresent()) {
            mergeAuthEntry(apiAuthEntry.get(), requestAuthEntry.get());
        }

        requestAuthEntry = findAuthEntry(request);

        if(!requestAuthEntry.isPresent()) {
            return Optional.empty();
        }

        expandAuthEntry(settings, requestAuthEntry.get(), defaults);

        if(isBlank(requestAuthEntry.get().getTokenUrl())) {
            throw new CommandException("'tokenUrl' is required for token authorization");
        }
        if(isBlank(requestAuthEntry.get().getClientId())) {
            throw new CommandException("'clientId' is required for token authorization");
        }
        if(isBlank(requestAuthEntry.get().getClientSecret())) {
            throw new CommandException("'clientSecret' is required for token authorization");
        }
        if(isBlank(requestAuthEntry.get().getScope())) {
            throw new CommandException("'scope' is required for token authorization");
        }

        return Optional.of(instanceRequest(settings, request.getName(), requestAuthEntry.get()));
    }

    @Override
    public void cleanupAuth(Settings settings, HTTPRequestEntry api, HTTPRequestEntry request) {
        Optional<HTTPTokenAuthEntry> requestAuthEntry = findAuthEntry(request);

        if(!requestAuthEntry.isPresent()) {
            return;
        }

        String clientId = requestAuthEntry.get().getClientId();
        removeJsonProperties(settings.getOverrideFilePath(),
            toGeneratedParam(type(), clientId, "expiration-millis"),
            toGeneratedParam(type(), clientId, "access-token"),
            toGeneratedParam(type(), clientId, "expires-in." + settings.getAuthBearerExpiresInTimeUnit()),
            toGeneratedParam(type(), clientId, "expiration-date"));
}

    @Override
    public void mergeAuthEntry(HTTPRequestEntry api, HTTPRequestEntry request) {
        Optional<HTTPTokenAuthEntry> apiAuthEntry = findAuthEntry(api);
        Optional<HTTPTokenAuthEntry> requestAuthEntry = findAuthEntry(request);

        if(!apiAuthEntry.isPresent() || !requestAuthEntry.isPresent()) {
            return;
        }

        mergeAuthEntry(apiAuthEntry.get(), requestAuthEntry.get());
    }

    @Override
    public List<Pair<String, String>> getAuthHeaders(Settings settings, HTTPRequestEntry request) {
        Optional<HTTPTokenAuthEntry> authEntry = findAuthEntry(request);
        List<Pair<String, String>> headers = new ArrayList<>();

        if(!authEntry.isPresent()) {
            return headers;
        }

        String tokenParam = toGeneratedParam(type(), authEntry.get().getClientId(), "access-token");
        String token = settings.getOrDefault(tokenParam, "");

        if(isBlank(token)) {
            LOGGER.fine("Bearer token was not generated for client ID: " + authEntry.get().getClientId());
        } else {
            LOGGER.fine("Found token authorization - clientId=" + authEntry.get().getClientId()
                + " token=" + token + " param=" + tokenParam);
            headers.add(new Pair<>("Authorization", "Bearer " + token));
        }

        return headers;
    }

    private void expandAuthEntry(Settings settings, HTTPTokenAuthEntry authEntry,
            Map<String, Object> authDefaults) {
        StringExpander stringExpander = new StringExpander(settings, authDefaults);

        authEntry.setTokenUrl(stringExpander.replaceAllInContent(authEntry.getTokenUrl()));
        authEntry.setGrantType(stringExpander.replaceAllInContent(authEntry.getGrantType()));
        authEntry.setClientId(stringExpander.replaceAllInContent(authEntry.getClientId()));
        authEntry.setClientSecret(stringExpander.replaceAllInContent(authEntry.getClientSecret()));
        authEntry.setScope(stringExpander.replaceAllInContent(authEntry.getScope()));
    }

    private HTTPRequestEntry instanceRequest(Settings settings, String requestName,
            HTTPTokenAuthEntry authEntry) {
        HTTPRequestEntry authRequest = new HTTPRequestEntry();

        authRequest.setName(requestName + "/token-authorization");
        authRequest.setMethod(settings.getAuthBearerRequestMethod());
        authRequest.setUrl(authEntry.getTokenUrl());
        authRequest.getHeaders().put("Content-Type", settings.getAuthBearerContentType());

        Map<String, String> bodyParams = new HashMap<>();
        bodyParams.put("grantTypeFieldName", settings.getAuthBearerGrantTypeFieldName());
        bodyParams.put("grantType", authEntry.getGrantType());
        bodyParams.put("clientIdFieldName", settings.getAuthBearerClientIdFieldName());
        bodyParams.put("clientId", authEntry.getClientId());
        bodyParams.put("clientSecretFieldName", settings.getAuthBearerClientSecretFieldName());
        bodyParams.put("clientSecret", authEntry.getClientSecret());
        bodyParams.put("scopeFieldName", settings.getAuthBearerScopeFieldName());
        bodyParams.put("scope", authEntry.getScope());

        StringExpander stringExpander = new StringExpander(new Settings());
        authRequest.setBodyContent(stringExpander.replaceAllInContent(bodyParams, settings.getAuthBearerBodyTemplate()));

        String expirationMillisParam = toGeneratedParam(settings.getRequestType(),
            authEntry.getClientId(), "expiration-millis");
        String tokenParam = toGeneratedParam(settings.getRequestType(),
            authEntry.getClientId(), "access-token");
        String expiresInUnitParam = toGeneratedParam(settings.getRequestType(),
            authEntry.getClientId(), "expires-in." + settings.getAuthBearerExpiresInTimeUnit());
        String expirationDateParam = toGeneratedParam(settings.getRequestType(),
            authEntry.getClientId(), "expiration-date");

        AssertionEntry expirationCondition = new AssertionEntry();
        expirationCondition.setName("LESS_THAN");
        expirationCondition.setArgs(new String[] {
            "{{:default:0:" + expirationMillisParam + "}}",
            "{{DATE-TIME:date-epoch:ISO_LOCAL_DATE_TIME:MILLIS:}}"
        });
        expirationCondition.setType(AssertionType.CONDITION);
        authRequest.getConditions().add(expirationCondition);

        authRequest.getOutputMappings().put(tokenParam, "{{OUT/" + settings.getAuthBearerAccessTokenFieldName() + "}}");
        authRequest.getOutputMappings().put(expiresInUnitParam, "{{OUT/" + settings.getAuthBearerExpiresInFieldName() + "}}");
        authRequest.getOutputMappings().put(expirationDateParam, "{{DATE-TIME:date-plus:yyyy-MM-dd'T'HH\\:mm\\:ss.n:"
            + settings.getAuthBearerExpiresInTimeUnit() + ":" + expiresInUnitParam + ":}}");
        authRequest.getOutputMappings().put(expirationMillisParam, "{{:date-epoch:ISO_LOCAL_DATE_TIME:MILLIS:" + expirationDateParam +"}}");

        AssertionEntry http200Assertion = new AssertionEntry();
        http200Assertion.setName("EQUALS_TO");
        http200Assertion.setArgs(new String[] {
            "200",
            "{{HTTP/status}}"
        });
        http200Assertion.setType(AssertionType.ASSERTION);
        authRequest.getAssertions().add(http200Assertion);

        return authRequest;
    }

    private void mergeAuthEntry(HTTPTokenAuthEntry api, HTTPTokenAuthEntry request) {
        if(isBlank(request.getTokenUrl())) {
            request.setTokenUrl(api.getTokenUrl());
        }
        if(isBlank(request.getGrantType())) {
            request.setGrantType(api.getGrantType());
        }
        if(isBlank(request.getClientId())) {
            request.setClientId(api.getClientId());
        }
        if(isBlank(request.getClientSecret())) {
            request.setClientSecret(api.getClientSecret());
        }
        if(isBlank(request.getScope())) {
            request.setScope(api.getScope());
        }
    }
}
