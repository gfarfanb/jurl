package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_MERGE_BODY_USING_TYPE;
import static com.legadi.cli.jurl.common.WriterUtils.writeFile;
import static com.legadi.cli.jurl.executor.http.HTTPRequestModifier.BODY_TEMPORAL_PATH;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.cli.jurl.assertions.EqualsToAssertionFunction;
import com.legadi.cli.jurl.common.ObjectsRegistry;
import com.legadi.cli.jurl.common.OutputPathBuilder;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.exception.RequestException;
import com.legadi.cli.jurl.executor.HeaderAuthenticator;
import com.legadi.cli.jurl.executor.RequestModifier;
import com.legadi.cli.jurl.model.AssertionEntry;
import com.legadi.cli.jurl.model.FlowEntry;
import com.legadi.cli.jurl.model.RequestInput;
import com.legadi.cli.jurl.model.StepEntry;
import com.legadi.cli.jurl.model.http.HTTPMockEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPBasicAuthEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;
import com.legadi.cli.jurl.options.Option;
import com.legadi.cli.jurl.options.OptionsReader;
import com.legadi.cli.jurl.options.OptionsReader.OptionEntry;
import com.legadi.cli.jurl.options.SetValueOption;

public class HTTPRequestModifierTest {

    @Test
    public void getAuthenticationDefinitionValidation() {
        Settings settings = new Settings();
        String requestName = UUID.randomUUID().toString();

        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setName(requestName);
        requestInput.setApi(new HTTPRequestEntry());
        requestInput.getRequests().put(requestName, request);

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        List<?> authRequests = modifier.getAuthenticationIfExists(requestName,
            requestInput, settings);

        Assertions.assertTrue(authRequests.isEmpty());
    }

    @Test
    public void getAuthenticationDefinitionRequestAuth() {
        Settings settings = new Settings();
        HeaderAuthenticator<?, ?> tokenAuthenticator = ObjectsRegistry.<HeaderAuthenticator<?, ?>>findAll(HeaderAuthenticator.class, "http")
            .stream()
            .filter(auth -> auth.getParserElement().equalsIgnoreCase("token"))
            .findFirst()
            .get();
        String requestName = UUID.randomUUID().toString();
        String secret = UUID.randomUUID().toString();

        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        HTTPRequestEntry request = new HTTPRequestEntry();
        HTTPTokenAuthEntry requestAuth = (HTTPTokenAuthEntry) tokenAuthenticator.instanceAuthEntry(settings);

        requestAuth.setTokenUrl("http://localhost:555555/oauth/token");
        requestAuth.setClientId("flow-spec-client-id");
        requestAuth.setClientSecret(secret);
        requestAuth.setScope("test");

        request.setName(requestName);
        request.getAuthEntries().put(requestAuth.getParserElement(), requestAuth);

        requestInput.setApi(new HTTPRequestEntry());
        requestInput.getRequests().put(requestName, request);

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        List<?> authRequests = modifier.getAuthenticationIfExists(requestName,
            requestInput, settings);

        Assertions.assertFalse(authRequests.isEmpty());

        HTTPRequestEntry authRequest = (HTTPRequestEntry) authRequests.get(0);

        Assertions.assertEquals(requestName + "/token-authorization", authRequest.getName());
        Assertions.assertEquals("POST", authRequest.getMethod());
        Assertions.assertEquals("http://localhost:555555/oauth/token", authRequest.getUrl());
        Assertions.assertEquals("application/x-www-form-urlencoded", authRequest.getHeaders().get("Content-Type"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("grant_type"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("client_credentials"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("client_id"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("flow-spec-client-id"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("client_secret"));
        Assertions.assertTrue(authRequest.getBodyContent().contains(secret));
        Assertions.assertTrue(authRequest.getBodyContent().contains("scope"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("test"));
        Assertions.assertEquals(1, authRequest.getConditions().size());
        Assertions.assertEquals(4, authRequest.getOutputMappings().size());
        Assertions.assertEquals(1, authRequest.getAssertions().size());
    }

    @Test
    public void getAuthenticationDefinitionMergeAuth() {
        Settings settings = new Settings();
        HeaderAuthenticator<?, ?> tokenAuthenticator = ObjectsRegistry.<HeaderAuthenticator<?, ?>>findAll(HeaderAuthenticator.class, "http")
            .stream()
            .filter(auth -> auth.getParserElement().equalsIgnoreCase("token"))
            .findFirst()
            .get();
        String requestName = UUID.randomUUID().toString();
        String secret = UUID.randomUUID().toString();

        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        HTTPRequestEntry request = new HTTPRequestEntry();
        HTTPTokenAuthEntry requestAuth = (HTTPTokenAuthEntry) tokenAuthenticator.instanceAuthEntry(settings);

        requestAuth.setTokenUrl("http://localhost:555555/oauth/token");
        requestAuth.setClientId("flow-spec-client-id");
        requestAuth.setClientSecret(secret);
        requestAuth.setScope("test");

        request.setName(requestName);
        request.getAuthEntries().put("token", new HTTPTokenAuthEntry());

        requestInput.setApi(new HTTPRequestEntry());
        requestInput.getApi().getAuthEntries().put(requestAuth.getParserElement(), requestAuth);
        requestInput.getRequests().put(requestName, request);

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        List<?> authRequests = modifier.getAuthenticationIfExists(requestName,
            requestInput, settings);

        Assertions.assertFalse(authRequests.isEmpty());

        HTTPRequestEntry authRequest = (HTTPRequestEntry) authRequests.get(0);

        Assertions.assertEquals(requestName + "/token-authorization", authRequest.getName());
        Assertions.assertEquals("POST", authRequest.getMethod());
        Assertions.assertEquals("http://localhost:555555/oauth/token", authRequest.getUrl());
        Assertions.assertEquals("application/x-www-form-urlencoded", authRequest.getHeaders().get("Content-Type"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("grant_type"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("client_credentials"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("client_id"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("flow-spec-client-id"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("client_secret"));
        Assertions.assertTrue(authRequest.getBodyContent().contains(secret));
        Assertions.assertTrue(authRequest.getBodyContent().contains("scope"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("test"));
        Assertions.assertEquals(1, authRequest.getConditions().size());
        Assertions.assertEquals(4, authRequest.getOutputMappings().size());
        Assertions.assertEquals(1, authRequest.getAssertions().size());
    }

    @Test
    public void getAuthenticationDefinitionMergeAuthRequestPriority() {
        Settings settings = new Settings();
        HeaderAuthenticator<?, ?> tokenAuthenticator = ObjectsRegistry.<HeaderAuthenticator<?, ?>>findAll(HeaderAuthenticator.class, "http")
            .stream()
            .filter(auth -> auth.getParserElement().equalsIgnoreCase("token"))
            .findFirst()
            .get();
        String requestName = UUID.randomUUID().toString();
        String secret = UUID.randomUUID().toString();

        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        HTTPRequestEntry request = new HTTPRequestEntry();
        HTTPTokenAuthEntry requestAuth = (HTTPTokenAuthEntry) tokenAuthenticator.instanceAuthEntry(settings);

        requestAuth.setTokenUrl("http://localhost:555555/oauth/token");
        requestAuth.setClientId("flow-spec-client-id");
        requestAuth.setClientSecret(secret);
        requestAuth.setScope("test");

        request.setName(requestName);
        request.getAuthEntries().put(requestAuth.getParserElement(), requestAuth);

        HTTPTokenAuthEntry apiAuth = (HTTPTokenAuthEntry) tokenAuthenticator.instanceAuthEntry(settings);

        apiAuth.setTokenUrl("http://api:555555/oauth/token");
        apiAuth.setClientId("api-flow-spec-client-id");
        apiAuth.setClientSecret("api" + secret);
        apiAuth.setScope("api-test");

        requestInput.setApi(new HTTPRequestEntry());
        requestInput.getApi().getAuthEntries().put(apiAuth.getParserElement(), apiAuth);
        requestInput.getRequests().put(requestName, request);

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        List<?> authRequests = modifier.getAuthenticationIfExists(requestName,
            requestInput, settings);

        Assertions.assertFalse(authRequests.isEmpty());

        HTTPRequestEntry authRequest = (HTTPRequestEntry) authRequests.get(0);

        Assertions.assertEquals(requestName + "/token-authorization", authRequest.getName());
        Assertions.assertEquals("POST", authRequest.getMethod());
        Assertions.assertEquals("http://localhost:555555/oauth/token", authRequest.getUrl());
        Assertions.assertEquals("application/x-www-form-urlencoded", authRequest.getHeaders().get("Content-Type"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("grant_type"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("client_credentials"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("client_id"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("flow-spec-client-id"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("client_secret"));
        Assertions.assertTrue(authRequest.getBodyContent().contains(secret));
        Assertions.assertTrue(authRequest.getBodyContent().contains("scope"));
        Assertions.assertTrue(authRequest.getBodyContent().contains("test"));
        Assertions.assertEquals(1, authRequest.getConditions().size());
        Assertions.assertEquals(4, authRequest.getOutputMappings().size());
        Assertions.assertEquals(1, authRequest.getAssertions().size());
    }

    @Test
    public void getAuthenticationDefinitionWithEmptySpec() {
        Settings settings = new Settings();
        HeaderAuthenticator<?, ?> tokenAuthenticator = ObjectsRegistry.<HeaderAuthenticator<?, ?>>findAll(HeaderAuthenticator.class, "http")
            .stream()
            .filter(auth -> auth.getParserElement().equalsIgnoreCase("token"))
            .findFirst()
            .get();
        String requestName = UUID.randomUUID().toString();
        String secret = UUID.randomUUID().toString();

        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        HTTPRequestEntry request = new HTTPRequestEntry();
        HTTPTokenAuthEntry requestAuth = (HTTPTokenAuthEntry) tokenAuthenticator.instanceAuthEntry(settings);

        request.setName(requestName);
        request.getAuthEntries().put(requestAuth.getParserElement(), requestAuth);

        requestInput.setApi(new HTTPRequestEntry());
        requestInput.getRequests().put(requestName, request);

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");

        Assertions.assertThrows(CommandException.class, () ->
            modifier.getAuthenticationIfExists(requestName, requestInput, settings));

        requestAuth.setTokenUrl("http://localhost:555555/oauth/token");
        Assertions.assertThrows(CommandException.class, () ->
            modifier.getAuthenticationIfExists(requestName, requestInput, settings));

        requestAuth.setClientId("api-flow-spec-client-id");
        Assertions.assertThrows(CommandException.class, () ->
            modifier.getAuthenticationIfExists(requestName, requestInput, settings));

        requestAuth.setClientSecret(secret);
        Assertions.assertThrows(CommandException.class, () ->
            modifier.getAuthenticationIfExists(requestName, requestInput, settings));

        requestAuth.setScope("test");

        List<?> authRequests = modifier.getAuthenticationIfExists(requestName,
            requestInput, settings);

        Assertions.assertFalse(authRequests.isEmpty());
    }

    @Test
    public void mergeRequestHeaderValidation() {
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setUrl("https://localhost:9876");
        api.setProtocol("https");
        api.setPort("9876");
        api.setHost("localhost:9876");
        api.setBasePath("/api");
        api.setEndpoint("/v1");

        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setUrl("http://localhost:1234");
        request.setProtocol("http");
        request.setHost("localhost:1234");
        request.setPort("1234");
        request.setBasePath("/base");
        request.setEndpoint("/endpoint");

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeHeader(api, request);

        Assertions.assertEquals("http://localhost:1234", request.getUrl());
        Assertions.assertEquals("http", request.getProtocol());
        Assertions.assertEquals("localhost:1234", request.getHost());
        Assertions.assertEquals("1234", request.getPort());
        Assertions.assertEquals("/base", request.getBasePath());
        Assertions.assertEquals("/endpoint", request.getEndpoint());
    }

    @Test
    public void mergeRequestHeaderAPIValues() {
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setUrl("https://localhost:9876");
        api.setProtocol("https");
        api.setHost("localhost:9876");
        api.setPort("9876");
        api.setBasePath("/api");
        api.setEndpoint("/v1");

        HTTPRequestEntry request = new HTTPRequestEntry();

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeHeader(api, request);

        Assertions.assertEquals("https://localhost:9876", request.getUrl());
        Assertions.assertEquals("https", request.getProtocol());
        Assertions.assertEquals("localhost:9876", request.getHost());
        Assertions.assertEquals("9876", request.getPort());
        Assertions.assertEquals("/api", request.getBasePath());
        Assertions.assertEquals("/v1", request.getEndpoint());
    }

    @Test
    public void mergeAPIDefinitionValidation() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setMockDefinition(new HTTPMockEntry());

        AssertionEntry assertionEntry = new AssertionEntry();
        assertionEntry.setName("TEST");

        api.getConditions().add(assertionEntry);
        api.getOutputMappings().put("mapping.property", "{{mapping}}");
        api.getAssertions().add(assertionEntry);

        OptionEntry option = new OptionEntry(findByNameOrFail(Option.class, "-h"), new String[0]);

        api.getOptions().add(option);

        api.setMethod("POST");
        api.getQueryParams().put("param", "param-value");
        api.getHeaders().put("header", "header-value");
        api.setBodyCharset(StandardCharsets.UTF_16.name());
        api.setBodyContent("{}");
        api.setBodyFilePath("path/");
        api.getRequestFiles().add(new HTTPRequestFileEntry());

        HTTPRequestEntry request = new HTTPRequestEntry();

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeAPI(settings, api, request);

        Assertions.assertNotNull(request.getMockDefinition());
        Assertions.assertFalse(request.getConditions().isEmpty());
        Assertions.assertEquals("TEST", request.getConditions().get(0).getName());
        Assertions.assertFalse(request.getOutputMappings().isEmpty());
        Assertions.assertEquals("{{mapping}}", request.getOutputMappings().get("mapping.property"));
        Assertions.assertFalse(request.getAssertions().isEmpty());
        Assertions.assertEquals("TEST", request.getAssertions().get(0).getName());
        Assertions.assertFalse(request.getOptions().isEmpty());
        Assertions.assertEquals("--help", request.getOptions().get(0).getLeft().name());

        Assertions.assertEquals("POST", request.getMethod());
        Assertions.assertFalse(request.getQueryParams().isEmpty());
        Assertions.assertEquals("param-value", request.getQueryParams().get("param"));
        Assertions.assertFalse(request.getHeaders().isEmpty());
        Assertions.assertEquals("header-value", request.getHeaders().get("header"));
        Assertions.assertEquals(StandardCharsets.UTF_16.name(), request.getBodyCharset());
        Assertions.assertEquals("{}", request.getBodyContent());
        Assertions.assertEquals("path/", request.getBodyFilePath());
        Assertions.assertFalse(request.getRequestFiles().isEmpty());
    }

    @Test
    public void mergeAPIDefinitionRequestValues() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();
        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setMockDefinition(new HTTPMockEntry());
        request.setMethod("POST");
        request.setBodyCharset(StandardCharsets.UTF_16.name());
        request.setBodyContent("{}");
        request.setBodyFilePath("path/");
        request.getRequestFiles().add(new HTTPRequestFileEntry());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeAPI(settings, api, request);

        Assertions.assertNotNull(request.getMockDefinition());
        Assertions.assertEquals("POST", request.getMethod());
        Assertions.assertEquals(StandardCharsets.UTF_16.name(), request.getBodyCharset());
        Assertions.assertEquals("{}", request.getBodyContent());
        Assertions.assertEquals("path/", request.getBodyFilePath());
        Assertions.assertFalse(request.getRequestFiles().isEmpty());
    }

    @Test
    public void mergeAPIDefinitionMockDefinition() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setMockDefinition(new HTTPMockEntry());
        api.getMockDefinition().setStatusCode("200");
        api.getMockDefinition().setSecondsDelay("5");
        api.getMockDefinition().getResponseHeaders().put("header", "header-value");
        api.getMockDefinition().setResponseContent("{}");
        api.getMockDefinition().setResponseFilePath("path/");
        api.getMockDefinition().setExceptionClassOnOutputStream(IOException.class.getName());
        api.getMockDefinition().setExceptionClassOnResponseCode(IOException.class.getName());

        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setMockDefinition(new HTTPMockEntry());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeAPI(settings, api, request);

        Assertions.assertEquals("200", request.getMockDefinition().getStatusCode());
        Assertions.assertEquals("5", request.getMockDefinition().getSecondsDelay());
        Assertions.assertFalse(request.getMockDefinition().getResponseHeaders().isEmpty());
        Assertions.assertEquals("header-value", request.getMockDefinition().getResponseHeaders().get("header"));
        Assertions.assertEquals("{}", request.getMockDefinition().getResponseContent());
        Assertions.assertEquals("path/", request.getMockDefinition().getResponseFilePath());
        Assertions.assertEquals(IOException.class.getName(), request.getMockDefinition().getExceptionClassOnOutputStream());
        Assertions.assertEquals(IOException.class.getName(), request.getMockDefinition().getExceptionClassOnResponseCode());
    }

    @Test
    public void mergeAPIDefinitionMockDefinitionRequestValues() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setMockDefinition(new HTTPMockEntry());

        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setMockDefinition(new HTTPMockEntry());
        request.getMockDefinition().setStatusCode("200");
        request.getMockDefinition().setSecondsDelay("5");
        request.getMockDefinition().getResponseHeaders().put("header", "header-value");
        request.getMockDefinition().setResponseContent("{}");
        request.getMockDefinition().setResponseFilePath("path/");
        request.getMockDefinition().setExceptionClassOnOutputStream(IOException.class.getName());
        request.getMockDefinition().setExceptionClassOnResponseCode(IOException.class.getName());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeAPI(settings, api, request);

        Assertions.assertEquals("200", request.getMockDefinition().getStatusCode());
        Assertions.assertEquals("5", request.getMockDefinition().getSecondsDelay());
        Assertions.assertFalse(request.getMockDefinition().getResponseHeaders().isEmpty());
        Assertions.assertEquals("header-value", request.getMockDefinition().getResponseHeaders().get("header"));
        Assertions.assertEquals("{}", request.getMockDefinition().getResponseContent());
        Assertions.assertEquals("path/", request.getMockDefinition().getResponseFilePath());
        Assertions.assertEquals(IOException.class.getName(), request.getMockDefinition().getExceptionClassOnOutputStream());
        Assertions.assertEquals(IOException.class.getName(), request.getMockDefinition().getExceptionClassOnResponseCode());
    }

    @Test
    public void mergeAPIDefinitionRequestFile() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.getRequestFiles().add(new HTTPRequestFileEntry());
        api.getRequestFiles().get(0).setPath("path/");
        api.getRequestFiles().get(0).setName("file");
        api.getRequestFiles().get(0).setField("field");
        api.getRequestFiles().get(0).setMineType("application/xml");

        api.getFormData().put("field", "field-value");

        HTTPRequestEntry request = new HTTPRequestEntry();

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeAPI(settings, api, request);

        Assertions.assertEquals("path/", request.getRequestFiles().get(0).getPath());
        Assertions.assertEquals("file", request.getRequestFiles().get(0).getName());
        Assertions.assertEquals("field", request.getRequestFiles().get(0).getField());
        Assertions.assertEquals("application/xml", request.getRequestFiles().get(0).getMineType());
        Assertions.assertFalse(request.getFormData().isEmpty());
        Assertions.assertEquals("field-value", request.getFormData().get("field"));
    }

    @Test
    public void mergeAPIDefinitionRequestFileRequestValues() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.getRequestFiles().add(new HTTPRequestFileEntry());
        api.getRequestFiles().get(0).setPath("path/");
        api.getRequestFiles().get(0).setName("file");
        api.getRequestFiles().get(0).setField("field");
        api.getRequestFiles().get(0).setMineType("application/xml");

        api.getRequestFiles().add(new HTTPRequestFileEntry());
        api.getRequestFiles().get(1).setPath("path/1");

        HTTPRequestEntry request = new HTTPRequestEntry();

        request.getRequestFiles().add(new HTTPRequestFileEntry());
        request.getRequestFiles().get(0).setPath("path/");

        request.getRequestFiles().add(new HTTPRequestFileEntry());
        request.getRequestFiles().get(1).setPath("path/1");
        request.getRequestFiles().get(1).setName("file");
        request.getRequestFiles().get(1).setField("field");
        request.getRequestFiles().get(1).setMineType("application/xml");

        request.getFormData().put("field", "field-value");

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeAPI(settings, api, request);

        Assertions.assertEquals("path/", request.getRequestFiles().get(0).getPath());
        Assertions.assertEquals("file", request.getRequestFiles().get(0).getName());
        Assertions.assertEquals("field", request.getRequestFiles().get(0).getField());
        Assertions.assertEquals("application/xml", request.getRequestFiles().get(0).getMineType());
        Assertions.assertFalse(request.getFormData().isEmpty());
        Assertions.assertEquals("field-value", request.getFormData().get("field"));
    }

    @Test
    public void mergeAPIDefinitionNullRequestsObjects() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();
        HTTPRequestEntry request = new HTTPRequestEntry();

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");

        Assertions.assertDoesNotThrow(() -> modifier.mergeAPI(settings, null, request));
        Assertions.assertDoesNotThrow(() -> modifier.mergeAPI(settings, api, null));
    }

    @Test
    public void mergeAPIDefinitionAPIBasicAuth() {
        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        Settings settings = new Settings();

        HTTPRequestEntry api;
        HTTPRequestEntry request;
        HTTPBasicAuthEntry apiBasicAuth;
        HTTPBasicAuthEntry requestBasicAuth;

        api = new HTTPRequestEntry();
        request = new HTTPRequestEntry();

        apiBasicAuth = new HTTPBasicAuthEntry();
        apiBasicAuth.setUsername("api-username");
        apiBasicAuth.setPassword("api-password");
        api.getAuthEntries().put(apiBasicAuth.getParserElement(), apiBasicAuth);

        modifier.mergeAPI(settings, api, request);

        requestBasicAuth = (HTTPBasicAuthEntry) request.getAuthEntries().get("basic");

        Assertions.assertEquals("api-username", requestBasicAuth.getUsername());
        Assertions.assertEquals("api-password", requestBasicAuth.getPassword());

        api = new HTTPRequestEntry();
        request = new HTTPRequestEntry();

        requestBasicAuth = new HTTPBasicAuthEntry();
        requestBasicAuth.setUsername("request-username");
        requestBasicAuth.setPassword("request-password");
        request.getAuthEntries().put(requestBasicAuth.getParserElement(), requestBasicAuth);

        modifier.mergeAPI(settings, api, request);

        requestBasicAuth = (HTTPBasicAuthEntry) request.getAuthEntries().get("basic");

        Assertions.assertEquals("request-username", requestBasicAuth.getUsername());
        Assertions.assertEquals("request-password", requestBasicAuth.getPassword());

        api = new HTTPRequestEntry();
        request = new HTTPRequestEntry();

        apiBasicAuth = new HTTPBasicAuthEntry();
        apiBasicAuth.setUsername("api-username");
        apiBasicAuth.setPassword("api-password");
        api.getAuthEntries().put(apiBasicAuth.getParserElement(), apiBasicAuth);

        requestBasicAuth = new HTTPBasicAuthEntry();
        requestBasicAuth.setUsername("request-username");
        requestBasicAuth.setPassword("request-password");
        request.getAuthEntries().put(requestBasicAuth.getParserElement(), requestBasicAuth);

        modifier.mergeAPI(settings, api, request);

        requestBasicAuth = (HTTPBasicAuthEntry) request.getAuthEntries().get("basic");

        Assertions.assertEquals("request-username", requestBasicAuth.getUsername());
        Assertions.assertEquals("request-password", requestBasicAuth.getPassword());

        api = new HTTPRequestEntry();
        request = new HTTPRequestEntry();

        apiBasicAuth = new HTTPBasicAuthEntry();
        apiBasicAuth.setUsername("api-username");
        apiBasicAuth.setPassword("api-password");
        api.getAuthEntries().put(apiBasicAuth.getParserElement(), apiBasicAuth);

        requestBasicAuth = new HTTPBasicAuthEntry();
        request.getAuthEntries().put(requestBasicAuth.getParserElement(), requestBasicAuth);

        modifier.mergeAPI(settings, api, request);

        requestBasicAuth = (HTTPBasicAuthEntry) request.getAuthEntries().get("basic");

        Assertions.assertEquals("api-username", requestBasicAuth.getUsername());
        Assertions.assertEquals("api-password", requestBasicAuth.getPassword());
    }

    @Test
    public void mergeAPIDefinitionAPITokenAuth() {
        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        Settings settings = new Settings();

        HTTPRequestEntry api;
        HTTPRequestEntry request;
        HTTPTokenAuthEntry apiTokenAuth;
        HTTPTokenAuthEntry requestTokenAuth;

        api = new HTTPRequestEntry();
        request = new HTTPRequestEntry();

        apiTokenAuth = new HTTPTokenAuthEntry();
        apiTokenAuth.setClientId("api-client-id");
        apiTokenAuth.setClientSecret("api-client-secret");
        api.getAuthEntries().put(apiTokenAuth.getParserElement(), apiTokenAuth);

        modifier.mergeAPI(settings, api, request);

        requestTokenAuth = (HTTPTokenAuthEntry) request.getAuthEntries().get("token");

        Assertions.assertEquals("api-client-id", requestTokenAuth.getClientId());
        Assertions.assertEquals("api-client-secret", requestTokenAuth.getClientSecret());

        api = new HTTPRequestEntry();
        request = new HTTPRequestEntry();

        requestTokenAuth = new HTTPTokenAuthEntry();
        requestTokenAuth.setClientId("request-client-id");
        requestTokenAuth.setClientSecret("request-client-secret");
        request.getAuthEntries().put(requestTokenAuth.getParserElement(), requestTokenAuth);

        modifier.mergeAPI(settings, api, request);

        requestTokenAuth = (HTTPTokenAuthEntry) request.getAuthEntries().get("token");

        Assertions.assertEquals("request-client-id", requestTokenAuth.getClientId());
        Assertions.assertEquals("request-client-secret", requestTokenAuth.getClientSecret());

        api = new HTTPRequestEntry();
        request = new HTTPRequestEntry();

        apiTokenAuth = new HTTPTokenAuthEntry();
        apiTokenAuth.setClientId("api-client-id");
        apiTokenAuth.setClientSecret("api-client-id");
        api.getAuthEntries().put(apiTokenAuth.getParserElement(), apiTokenAuth);

        requestTokenAuth = new HTTPTokenAuthEntry();
        requestTokenAuth.setClientId("request-client-id");
        requestTokenAuth.setClientSecret("request-client-secret");
        request.getAuthEntries().put(requestTokenAuth.getParserElement(), requestTokenAuth);

        modifier.mergeAPI(settings, api, request);

        requestTokenAuth = (HTTPTokenAuthEntry) request.getAuthEntries().get("token");

        Assertions.assertEquals("request-client-id", requestTokenAuth.getClientId());
        Assertions.assertEquals("request-client-secret", requestTokenAuth.getClientSecret());

        api = new HTTPRequestEntry();
        request = new HTTPRequestEntry();

        apiTokenAuth = new HTTPTokenAuthEntry();
        apiTokenAuth.setClientId("api-client-id");
        apiTokenAuth.setClientSecret("api-client-secret");
        api.getAuthEntries().put(apiTokenAuth.getParserElement(), apiTokenAuth);

        requestTokenAuth = new HTTPTokenAuthEntry();
        request.getAuthEntries().put(requestTokenAuth.getParserElement(), requestTokenAuth);

        modifier.mergeAPI(settings, api, request);

        requestTokenAuth = (HTTPTokenAuthEntry) request.getAuthEntries().get("token");

        Assertions.assertEquals("api-client-id", requestTokenAuth.getClientId());
        Assertions.assertEquals("api-client-secret", requestTokenAuth.getClientSecret());
    }

    @Test
    public void mergeBodyFileWithBodyContentValidation() {
        Settings settings = new Settings();

        settings.putOverride(PROP_MERGE_BODY_USING_TYPE, "json");

        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
                .setRequestPath("src/test/resources/http-request-modifier.http")
                .setRequestName("merge-body")
                .setExtension("content");
        Path bodyPath = pathBuilder.buildCommandPath();

        writeFile(bodyPath, "{\"name\": \"file-content\"}");

        HTTPRequestEntry request = new HTTPRequestEntry();
        request.setName("merge-body");
        request.setBodyContent("{\"name\": \"body-content\"}");
        request.setBodyFilePath(bodyPath.toString());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeBody(settings, "src/test/resources/http-request-modifier.http", request);

        Assertions.assertNull(request.getBodyContent());
        Assertions.assertNull(request.getBodyFilePath());

        String bodyTemporalPath = settings.get(BODY_TEMPORAL_PATH);

        Assertions.assertDoesNotThrow(() -> Paths.get(bodyTemporalPath));

        Map<String, Object> merged = loadJsonFile(bodyTemporalPath, new TypeToken<Map<String, Object>>() {});

        Assertions.assertTrue(merged.containsKey("name"));
        Assertions.assertEquals("body-content", merged.get("name"));
    }

    @Test
    public void mergeBodyFileWithBodyContentMissingData() {
        Settings settings = new Settings();

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();

        Assertions.assertThrows(RequestException.class,
            () -> modifier.mergeBody(settings, "src/test/resources/http-request-modifier.http", request));

        request.setBodyFilePath("body/file/path");

        Assertions.assertThrows(RequestException.class,
            () -> modifier.mergeBody(settings, "src/test/resources/http-request-modifier.http", request));
    }

    @Test
    public void overrideRequestWithFileValidation() {
        Settings settings = new Settings();
        HTTPRequestEntry request = new HTTPRequestEntry();

        request.getHeaders().put("Content-Type", "application/json");
        request.getQueryParams().put("param", "param-value");
        request.setBodyContent("{}");
        request.setBodyFilePath("path/");

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.overrideRequest(settings, request, "src/test/resources/http-request-modifier.request.txt");

        Assertions.assertEquals(1, request.getHeaders().size());
        Assertions.assertEquals("application/xml", request.getHeaders().get("Content-Type"));
        Assertions.assertEquals(1, request.getQueryParams().size());
        Assertions.assertDoesNotThrow(() -> UUID.fromString(request.getQueryParams().get("param")));
        Assertions.assertTrue(request.getBodyContent().contains("\"name\": \"request\""));
        Assertions.assertEquals("overrided/request/path", request.getBodyFilePath());
    }

    @Test
    public void overrideRequestWithFileEmpty() {
        Settings settings = new Settings();
        HTTPRequestEntry request = new HTTPRequestEntry();

        request.getHeaders().put("Content-Type", "application/json");
        request.getQueryParams().put("param", "param-value");
        request.setBodyContent("{}");
        request.setBodyFilePath("path/");

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.overrideRequest(settings, request, "src/test/resources/http-request-modifier.empty.txt");

        Assertions.assertEquals(1, request.getHeaders().size());
        Assertions.assertEquals("application/json", request.getHeaders().get("Content-Type"));
        Assertions.assertEquals(1, request.getQueryParams().size());
        Assertions.assertEquals("param-value", request.getQueryParams().get("param"));
        Assertions.assertEquals("{}", request.getBodyContent());
        Assertions.assertEquals("path/", request.getBodyFilePath());
    }

    @Test
    public void expandFlowValidation() {
        Settings settings = new Settings();
        FlowEntry flow = new FlowEntry();

        settings.putUserInput("default.int", "5");
        settings.putUserInput("request.input.path", "/path");

        flow.getDefaults().put("defaultInt", "{{default.int}}");

        flow.getSteps().add(new StepEntry());
        flow.getSteps().get(0).setRequestInputPath("{{request.input.path}}");
        flow.getSteps().get(0).setOptions(new OptionsReader(new String[] { "-s", "field", "{{default.int}}" })
            .getOptionEntries());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.expandFlow(settings, flow);

        Assertions.assertEquals("5", flow.getDefaults().get("defaultInt"));
        Assertions.assertEquals("/path", flow.getSteps().get(0).getRequestInputPath());
        Assertions.assertEquals("--set", flow.getSteps().get(0).getOptions().get(0).getLeft().name());
        Assertions.assertEquals("field", flow.getSteps().get(0).getOptions().get(0).getRight()[0]);
        Assertions.assertEquals("5", flow.getSteps().get(0).getOptions().get(0).getRight()[1]);
    }

    @Test
    public void expandFlowWithNullDefaults() {
        Settings settings = new Settings();
        FlowEntry flow = new FlowEntry();

        flow.setDefaults(null);

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.expandFlow(settings, flow);

        Assertions.assertNull(flow.getDefaults());
        Assertions.assertTrue(flow.getSteps().isEmpty());
    }

    @Test
    public void expandFlowWithNullArgs() {
        Settings settings = new Settings();
        FlowEntry flow = new FlowEntry();

        StepEntry step = new StepEntry();
        OptionEntry option = new OptionEntry(new SetValueOption(), null);

        List<OptionEntry> options = new ArrayList<>();
        options.add(option);
        step.setOptions(options);

        flow.getSteps().add(step);

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.expandFlow(settings, flow);

        Assertions.assertFalse(flow.getSteps().isEmpty());
    }

    @Test
    public void expandRequestValidation() {
        Settings settings = new Settings();
        HTTPRequestEntry request = new HTTPRequestEntry();
        HTTPMockEntry mock = new HTTPMockEntry();

        settings.putUserInput("url", "http://localhost:8080/path");
        settings.putUserInput("protocol", "http");
        settings.putUserInput("host", "localhost:8080");
        settings.putUserInput("port", "8080");
        settings.putUserInput("basePath", "/path");
        settings.putUserInput("endpoint", "/endpoint");
        settings.putUserInput("conditionName", "EQUALS_TO");
        settings.putUserInput("conditionClass", EqualsToAssertionFunction.class.getName());
        settings.putUserInput("conditionMessage", "Condition Failed");
        settings.putUserInput("conditionArg1", "left");
        settings.putUserInput("conditionArg2", "right");
        settings.putUserInput("optionArg1", "name");
        settings.putUserInput("optionArg2", "value");
        settings.putUserInput("method", "POST");
        settings.putUserInput("queryParam", "5");
        settings.putUserInput("headerAccept", "application/json");
        settings.putUserInput("bodyCharset", "UTF-8");
        settings.putUserInput("bodyContent", "{}");
        settings.putUserInput("bodyFilePath", "/body.json");
        settings.putUserInput("requestFileName", "file.csv");
        settings.putUserInput("requestFilePath", "/file.csv");
        settings.putUserInput("requestFileField", "file");
        settings.putUserInput("requestFileMineType", "text/csv");
        settings.putUserInput("formValue", "5");
        
        settings.putUserInput("mockHeaderAccept", "application/json");
        settings.putUserInput("mockResponseContent", "{}");
        settings.putUserInput("mockResponseFilePath", "/response.json");
        settings.putUserInput("mockExceptionClassOnOutputStream", RuntimeException.class.getName());
        settings.putUserInput("mockExceptionClassOnResponseCode", Exception.class.getName());

        request.setUrl("{{url}}");
        request.setProtocol("{{protocol}}");
        request.setHost("{{host}}");
        request.setPort("{{port}}");
        request.setBasePath("{{basePath}}");
        request.setEndpoint("{{endpoint}}");

        AssertionEntry condition = new AssertionEntry();
        condition.setName("{{conditionName}}");
        condition.setAssertionClass("{{conditionClass}}");
        condition.setMessage("{{conditionMessage}}");
        condition.setArgs(new String[] { "{{conditionArg1}}", "{{conditionArg2}}" });
        request.getConditions().add(condition);

        OptionEntry option = new OptionEntry(new SetValueOption(),
            new String[] { "{{optionArg1}}", "{{optionArg2}}" });
        request.getOptions().add(option);

        request.getDefaults().put("default.int", "5");
        request.getDefaults().put("pad.index", "0000{{default.int}}");
        request.getDefaults().put("indexes", Arrays.asList("1", "2", "3", "4", "{{default.int}}"));
        request.getDefaults().put("empty.val", null);

        request.setMethod("{{method}}");
        request.getQueryParams().put("param", "{{queryParam}}");
        request.getHeaders().put("Accept", "{{headerAccept}}");
        request.setBodyCharset("{{bodyCharset}}");
        request.setBodyContent("{{bodyContent}}");
        request.setBodyFilePath("{{bodyFilePath}}");

        HTTPRequestFileEntry requestFile = new HTTPRequestFileEntry();
        requestFile.setName("{{requestFileName}}");
        requestFile.setPath("{{requestFilePath}}");
        requestFile.setField("{{requestFileField}}");
        requestFile.setMineType("{{requestFileMineType}}");
        request.getRequestFiles().add(requestFile);

        request.getFormData().put("field", "{{formValue}}");

        mock.getResponseHeaders().put("Accept", "{{mockHeaderAccept}}");
        mock.setResponseContent("{{mockResponseContent}}");
        mock.setResponseFilePath("{{mockResponseFilePath}}");
        mock.setExceptionClassOnOutputStream("{{mockExceptionClassOnOutputStream}}");
        mock.setExceptionClassOnResponseCode("{{mockExceptionClassOnResponseCode}}");
        request.setMockDefinition(mock);

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.expandRequest(settings, request);

        Assertions.assertEquals("http://localhost:8080/path", request.getUrl());
        Assertions.assertEquals("http", request.getProtocol());
        Assertions.assertEquals("localhost:8080", request.getHost());
        Assertions.assertEquals("8080", request.getPort());
        Assertions.assertEquals("/path", request.getBasePath());
        Assertions.assertEquals("/endpoint", request.getEndpoint());
        Assertions.assertEquals("EQUALS_TO", request.getConditions().get(0).getName());
        Assertions.assertEquals(EqualsToAssertionFunction.class.getName(), request.getConditions().get(0).getAssertionClass());
        Assertions.assertEquals("Condition Failed", request.getConditions().get(0).getMessage());
        Assertions.assertEquals("left", request.getConditions().get(0).getArgs()[0]);
        Assertions.assertEquals("right", request.getConditions().get(0).getArgs()[1]);
        Assertions.assertEquals("name", request.getOptions().get(0).getRight()[0]);
        Assertions.assertEquals("value", request.getOptions().get(0).getRight()[1]);
        Assertions.assertEquals("5", request.getDefaults().get("default.int"));
        Assertions.assertEquals("00005", request.getDefaults().get("pad.index"));
        Assertions.assertEquals(Arrays.asList("1", "2", "3", "4", "5"), request.getDefaults().get("indexes"));
        Assertions.assertNull(request.getDefaults().get("empty.val"));
        Assertions.assertEquals("POST", request.getMethod());
        Assertions.assertEquals("5", request.getQueryParams().get("param"));
        Assertions.assertEquals("application/json", request.getHeaders().get("Accept"));
        Assertions.assertEquals("UTF-8", request.getBodyCharset());
        Assertions.assertEquals("{}", request.getBodyContent());
        Assertions.assertEquals("/body.json", request.getBodyFilePath());
        Assertions.assertEquals("file.csv", request.getRequestFiles().get(0).getName());
        Assertions.assertEquals("/file.csv", request.getRequestFiles().get(0).getPath());
        Assertions.assertEquals("file", request.getRequestFiles().get(0).getField());
        Assertions.assertEquals("text/csv", request.getRequestFiles().get(0).getMineType());
        Assertions.assertEquals("5", request.getFormData().get("field"));
        Assertions.assertEquals("application/json", request.getMockDefinition().getResponseHeaders().get("Accept"));
        Assertions.assertEquals("{}", request.getMockDefinition().getResponseContent());
        Assertions.assertEquals("/response.json", request.getMockDefinition().getResponseFilePath());
        Assertions.assertEquals(RuntimeException.class.getName(), request.getMockDefinition().getExceptionClassOnOutputStream());
        Assertions.assertEquals(Exception.class.getName(), request.getMockDefinition().getExceptionClassOnResponseCode());
    }
}
