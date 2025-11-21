package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.common.AnnotationsUtils.extractTypedType;
import static com.legadi.cli.jurl.common.CommonUtils.getAllFields;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.toGeneratedParam;
import static com.legadi.cli.jurl.common.JsonUtils.removeJsonProperties;
import static com.legadi.cli.jurl.common.annotations.Evaluable.Operation.EQUALS_IGNORE_CASE;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.StringExpander;
import com.legadi.cli.jurl.common.annotations.ConfigReplaceable;
import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Typed;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.executor.HeaderAuthenticator;
import com.legadi.cli.jurl.model.AssertionEntry;
import com.legadi.cli.jurl.model.AssertionType;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;

@Typed(type = "http")
@Evaluable(values = { "http" }, op = EQUALS_IGNORE_CASE)
@ConfigReplaceable(registeredProperties = {
    HTTPTokenHeaderAuthenticator.PROP_GRANT_TYPE,
    HTTPTokenHeaderAuthenticator.PROP_GRANT_TYPE_FIELD_NAME,
    HTTPTokenHeaderAuthenticator.PROP_CLIENT_ID_FIELD_NAME,
    HTTPTokenHeaderAuthenticator.PROP_CLIENT_SECRET_FIELD_NAME,
    HTTPTokenHeaderAuthenticator.PROP_SCOPE_FIELD_NAME,
    HTTPTokenHeaderAuthenticator.PROP_ACCESS_TOKEN_FIELD_NAME,
    HTTPTokenHeaderAuthenticator.PROP_EXPIRES_IN_FIELD_NAME,
    HTTPTokenHeaderAuthenticator.PROP_EXPIRES_IN_TIME_UNIT,
    HTTPTokenHeaderAuthenticator.PROP_TOKEN_TYPE_FIELD_NAME,
    HTTPTokenHeaderAuthenticator.PROP_REQUEST_METHOD,
    HTTPTokenHeaderAuthenticator.PROP_CONTENT_TYPE
})
public class HTTPTokenHeaderAuthenticator implements HeaderAuthenticator<HTTPRequestEntry, HTTPTokenAuthEntry> {

    private static final Logger LOGGER = Logger.getLogger(HTTPTokenHeaderAuthenticator.class.getName());

    private static final Map<String, Field> TOKEN_AUTH_FIELDS = getAllFields(HTTPTokenAuthEntry.class);

    public static final String PROP_GRANT_TYPE = "httpTokenHeaderAuthGrantType";
    public static final String PROP_GRANT_TYPE_FIELD_NAME = "httpTokenHeaderAuthGrantTypeFieldName";
    public static final String PROP_CLIENT_ID_FIELD_NAME = "httpTokenHeaderAuthClientIdFieldName";
    public static final String PROP_CLIENT_SECRET_FIELD_NAME = "httpTokenHeaderAuthClientSecretFieldName";
    public static final String PROP_SCOPE_FIELD_NAME = "httpTokenHeaderAuthScopeFieldName";
    public static final String PROP_ACCESS_TOKEN_FIELD_NAME = "httpTokenHeaderAuthAccessTokenFieldName";
    public static final String PROP_EXPIRES_IN_FIELD_NAME = "httpTokenHeaderAuthExpiresInFieldName";
    public static final String PROP_EXPIRES_IN_TIME_UNIT = "httpTokenHeaderAuthExpiresInTimeUnit";
    public static final String PROP_TOKEN_TYPE_FIELD_NAME = "httpTokenHeaderAuthTokenTypeFieldName";
    public static final String PROP_REQUEST_METHOD = "httpTokenHeaderAuthRequestMethod";
    public static final String PROP_CONTENT_TYPE = "httpTokenHeaderAuthContentType";

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
        authEntry.setGrantType(getGrantType(settings));
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

        return Optional.of(instanceRequest(settings, request.getName(), requestAuthEntry.get()));
    }

    @Override
    public void cleanupAuth(Settings settings, HTTPRequestEntry api, HTTPRequestEntry request) {
        Optional<HTTPTokenAuthEntry> requestAuthEntry = findAuthEntry(request);

        if(!requestAuthEntry.isPresent()) {
            return;
        }

        String clientId = requestAuthEntry.get().getClientId();
        String[] properties = new String[] {
            toGeneratedParam(extractTypedType(this), clientId, "expiration-millis"),
            toGeneratedParam(extractTypedType(this), clientId, "access-token"),
            toGeneratedParam(extractTypedType(this), clientId, "token-type"),
            toGeneratedParam(extractTypedType(this), clientId, "expires-in." + getExpiresInTimeUnit(settings)),
            toGeneratedParam(extractTypedType(this), clientId, "expiration-date")
        };

        removeJsonProperties(settings.getOverrideFilePath(), properties);
        settings.removeProperties(properties);
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

        String tokenParam = toGeneratedParam(extractTypedType(this), authEntry.get().getClientId(), "access-token");
        String typeParam = toGeneratedParam(extractTypedType(this), authEntry.get().getClientId(), "token-type");
        String token = settings.getOrDefault(tokenParam, "");
        String type = settings.getOrDefault(typeParam, "");

        if(isBlank(token)) {
            LOGGER.fine("Token authorization was not generated for client ID: " + authEntry.get().getClientId());
        } else {
            LOGGER.fine("Found token authorization - clientId=" + authEntry.get().getClientId()
                + " token=" + token + " param=" + tokenParam);
            headers.add(new Pair<>("Authorization", type + " " + token));
        }

        return headers;
    }

    public String getGrantTypeFieldName(Settings settings) {
        return settings.getOrDefault(PROP_GRANT_TYPE_FIELD_NAME, "grant_type");
    }

    public String getClientIdFieldName(Settings settings) {
        return settings.getOrDefault(PROP_CLIENT_ID_FIELD_NAME, "client_id");
    }

    public String getClientSecretFieldName(Settings settings) {
        return settings.getOrDefault(PROP_CLIENT_SECRET_FIELD_NAME, "client_secret");
    }

    public String getScopeFieldName(Settings settings) {
        return settings.getOrDefault(PROP_SCOPE_FIELD_NAME, "scope");
    }

    public String getAccessTokenFieldName(Settings settings) {
        return settings.getOrDefault(PROP_ACCESS_TOKEN_FIELD_NAME, "access_token");
    }

    public String getExpiresInFieldName(Settings settings) {
        return settings.getOrDefault(PROP_EXPIRES_IN_FIELD_NAME, "expires_in");
    }

    public String getTokenTypeFieldName(Settings settings) {
        return settings.getOrDefault(PROP_TOKEN_TYPE_FIELD_NAME, "token_type");
    }

    public String getGrantType(Settings settings) {
        return settings.getOrDefault(PROP_GRANT_TYPE, "client_credentials");
    }

    public String getExpiresInTimeUnit(Settings settings) {
        return settings.getOrDefault(PROP_EXPIRES_IN_TIME_UNIT, "SECONDS");
    }

    public String getRequestMethod(Settings settings) {
        return settings.getOrDefault(PROP_EXPIRES_IN_TIME_UNIT, "POST");
    }

    public String getContentType(Settings settings) {
        return settings.getOrDefault(PROP_EXPIRES_IN_TIME_UNIT, "application/x-www-form-urlencoded");
    }

    private void expandAuthEntry(Settings settings, HTTPTokenAuthEntry authEntry,
            Map<String, Object> authDefaults) {
        StringExpander stringExpander = new StringExpander(settings, authDefaults);

        authEntry.setTokenUrl(stringExpander.replaceAllInContent(authEntry.getTokenUrl()));
        authEntry.setGrantType(stringExpander.replaceAllInContent(authEntry.getGrantType()));
        authEntry.setClientId(stringExpander.replaceAllInContent(authEntry.getClientId()));
        authEntry.setClientSecret(stringExpander.replaceAllInContent(authEntry.getClientSecret()));
        authEntry.setScope(stringExpander.replaceAllInContent(authEntry.getScope()));

        for(Map.Entry<String, String> fieldEntry : authEntry.getOtherFields().entrySet()) {
            authEntry.putField(fieldEntry.getKey(), stringExpander.replaceAllInContent(fieldEntry.getValue()));
        }
    }

    private HTTPRequestEntry instanceRequest(Settings settings, String requestName,
            HTTPTokenAuthEntry authEntry) {
        HTTPRequestEntry authRequest = new HTTPRequestEntry();

        authRequest.setName(requestName + "/token-authorization");
        authRequest.setMethod(getRequestMethod(settings));
        authRequest.setUrl(authEntry.getTokenUrl());
        authRequest.getHeaders().put("Content-Type", getContentType(settings));
        authRequest.setBodyContent(buildBodyContent(settings, authEntry));

        String expirationMillisParam = toGeneratedParam(settings.getRequestType(),
            authEntry.getClientId(), "expiration-millis");
        String tokenParam = toGeneratedParam(settings.getRequestType(),
            authEntry.getClientId(), "access-token");
        String typeParam = toGeneratedParam(settings.getRequestType(),
            authEntry.getClientId(), "token-type");
        String expiresInUnitParam = toGeneratedParam(settings.getRequestType(),
            authEntry.getClientId(), "expires-in." + getExpiresInTimeUnit(settings));
        String expirationDateParam = toGeneratedParam(settings.getRequestType(),
            authEntry.getClientId(), "expiration-date");

        AssertionEntry expirationCondition = new AssertionEntry();
        expirationCondition.setName("LESS_THAN");
        expirationCondition.setArgs(new String[] {
            "{{:default:0:" + expirationMillisParam + "}}",
            "{{DATE_TIME:date_epoch:ISO_LOCAL_DATE_TIME:MILLIS:}}"
        });
        expirationCondition.setType(AssertionType.CONDITION);
        authRequest.getConditions().add(expirationCondition);

        authRequest.getOutputMappings().put(tokenParam, "{{OUT/" + getAccessTokenFieldName(settings) + "}}");
        authRequest.getOutputMappings().put(typeParam, "{{OUT/" + getTokenTypeFieldName(settings) + "}}");
        authRequest.getOutputMappings().put(expiresInUnitParam, "{{OUT/" + getExpiresInFieldName(settings) + "}}");
        authRequest.getOutputMappings().put(expirationDateParam, "{{DATE_TIME:date_plus:yyyy-MM-dd'T'HH\\:mm\\:ss.n:"
            + getExpiresInTimeUnit(settings) + ":" + expiresInUnitParam + ":}}");
        authRequest.getOutputMappings().put(expirationMillisParam, "{{:date_epoch:ISO_LOCAL_DATE_TIME:MILLIS:" + expirationDateParam +"}}");

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

    private String buildBodyContent(Settings settings, HTTPTokenAuthEntry authEntry) {
        Map<String, String> bodyParams = new LinkedHashMap<>();
        bodyParams.put(getGrantTypeFieldName(settings), authEntry.getGrantType());
        bodyParams.put(getClientIdFieldName(settings), authEntry.getClientId());
        bodyParams.put(getClientSecretFieldName(settings), authEntry.getClientSecret());

        if(isNotBlank(authEntry.getScope())) {
            bodyParams.put(getScopeFieldName(settings), authEntry.getScope());
        }

        for(String fieldName : authEntry.getFieldNames()) {
            bodyParams.put(fieldName, authEntry.getField(fieldName));
        }

        return bodyParams.entrySet()
            .stream()
            .map(e -> e.getKey() + "=" + e.getValue())
            .collect(Collectors.joining("&"));
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
        Map<String, String> otherFields = new HashMap<>(api.getOtherFields());
        otherFields.putAll(request.getOtherFields());
        request.setOtherFields(otherFields);
    }
}
