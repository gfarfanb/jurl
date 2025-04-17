package com.legadi.cli.jurl.parser;

import static com.legadi.cli.jurl.common.CommonUtils.getDefaultFieldIndex;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.assertions.EqualsToAssertionFunction;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.StringExpander;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.model.FlowEntry;
import com.legadi.cli.jurl.model.RequestInput;
import com.legadi.cli.jurl.model.StepEntry;
import com.legadi.cli.jurl.model.http.HTTPMockEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPBasicAuthEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;

public class HTTPRequestParserTest {

    @Test
    public void typeValidation() {
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertEquals("http", parser.type());
    }

    @Test
    public void parseAPI() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-api.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        RequestInput<HTTPRequestEntry> requestInput = Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));

        HTTPRequestEntry api = requestInput.getApi();

        Assertions.assertNotNull(api);
        Assertions.assertEquals("POST", api.getMethod());
        Assertions.assertEquals("http://localhost:5555/spec", api.getUrl());
        Assertions.assertEquals(1, api.getQueryParams().size());
        Assertions.assertEquals("value", api.getQueryParams().get("param"));
        Assertions.assertEquals(1, api.getConditions().size());
        Assertions.assertEquals("EQUALS_TO", api.getConditions().get(0).getName());
        Assertions.assertEquals(1, api.getHeaders().size());
        Assertions.assertEquals("application/json", api.getHeaders().get("Content-Type"));
        Assertions.assertNotNull(api.getBodyContent());
        Assertions.assertEquals(1, api.getOutputMappings().size());
        Assertions.assertEquals("{{HTTP/header.Resource-ID}}", api.getOutputMappings().get("basic.functions.id"));
        Assertions.assertEquals(2, api.getAssertions().size());
        Assertions.assertEquals("EQUALS_TO", api.getAssertions().get(0).getName());
        Assertions.assertEquals(EqualsToAssertionFunction.class.getName(), api.getAssertions().get(1).getAssertionClass());
        Assertions.assertEquals(2, api.getOptions().size());
        Assertions.assertEquals("--set", api.getOptions().get(0).getLeft().name());
        Assertions.assertEquals("--set", api.getOptions().get(1).getLeft().name());

        HTTPMockEntry mock = api.getMockDefinition();

        Assertions.assertNotNull(mock);
        Assertions.assertEquals("201", mock.getStatusCode());
        Assertions.assertEquals("{}", mock.getResponseContent());
        Assertions.assertEquals(1, mock.getResponseHeaders().size());
        Assertions.assertEquals("application/json", mock.getResponseHeaders().get("Accept"));

        HTTPBasicAuthEntry basicAuth = api.getAuthEntries()
            .stream()
            .filter(auth -> HTTPBasicAuthEntry.class.isAssignableFrom(auth.getClass()))
            .map(auth -> (HTTPBasicAuthEntry) auth)
            .findFirst()
            .get();

        Assertions.assertNotNull(basicAuth);
        Assertions.assertEquals("http-api-username", basicAuth.getUsername());
        Assertions.assertEquals("http-api-password", basicAuth.getPassword());

        HTTPTokenAuthEntry tokenAuth = api.getAuthEntries()
            .stream()
            .filter(auth -> HTTPTokenAuthEntry.class.isAssignableFrom(auth.getClass()))
            .map(auth -> (HTTPTokenAuthEntry) auth)
            .findFirst()
            .get();

        Assertions.assertNotNull(tokenAuth);
        Assertions.assertEquals("http://localhost:5555/oauth/token", tokenAuth.getTokenUrl());
        Assertions.assertEquals("http-api-client-id", tokenAuth.getClientId());
        Assertions.assertEquals("http-api-client-secret", tokenAuth.getClientSecret());
        Assertions.assertEquals("test", tokenAuth.getScope());
    }

    @Test
    public void parseWithoutSections() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.no-sections.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(CommandException.class,
            () -> parser.parseInput(settings, requestPath));
    }

    @Test
    public void parseRequest() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        RequestInput<HTTPRequestEntry> requestInput = Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));

        HTTPRequestEntry api = requestInput.getApi();

        Assertions.assertNotNull(api);
        Assertions.assertEquals("http", api.getProtocol());
        Assertions.assertEquals("localhost", api.getHost());
        Assertions.assertEquals("5555", api.getPort());
        Assertions.assertEquals("basic", api.getBasePath());

        HTTPRequestEntry request = requestInput.getRequests().get("request");

        Assertions.assertEquals("request", request.getName());
        Assertions.assertEquals("Request specification", request.getDescription());
        Assertions.assertEquals("POST", request.getMethod());
        Assertions.assertEquals("POST", request.getMethod());
        Assertions.assertEquals("http://localhost:5555/spec", request.getUrl());
        Assertions.assertEquals(1, request.getQueryParams().size());
        Assertions.assertEquals("value", request.getQueryParams().get("param"));
        Assertions.assertEquals(1, request.getConditions().size());
        Assertions.assertEquals("EQUALS_TO", request.getConditions().get(0).getName());
        Assertions.assertEquals(1, request.getHeaders().size());
        Assertions.assertEquals("application/json", request.getHeaders().get("Content-Type"));
        Assertions.assertNotNull(request.getBodyContent());
        Assertions.assertEquals(1, request.getOutputMappings().size());
        Assertions.assertEquals("{{HTTP/header.Resource-ID}}", request.getOutputMappings().get("basic.functions.id"));
        Assertions.assertEquals(2, request.getAssertions().size());
        Assertions.assertEquals("EQUALS_TO", request.getAssertions().get(0).getName());
        Assertions.assertEquals(EqualsToAssertionFunction.class.getName(), request.getAssertions().get(1).getAssertionClass());
        Assertions.assertEquals(2, request.getOptions().size());
        Assertions.assertEquals("--set", request.getOptions().get(0).getLeft().name());
        Assertions.assertEquals("--set", request.getOptions().get(1).getLeft().name());

        HTTPMockEntry mock = request.getMockDefinition();

        Assertions.assertNotNull(mock);
        Assertions.assertEquals("201", mock.getStatusCode());
        Assertions.assertEquals("{}", mock.getResponseContent());
        Assertions.assertEquals(1, mock.getResponseHeaders().size());
        Assertions.assertEquals("application/json", mock.getResponseHeaders().get("Accept"));

        HTTPTokenAuthEntry tokenAuth = request.getAuthEntries()
            .stream()
            .filter(auth -> HTTPTokenAuthEntry.class.isAssignableFrom(auth.getClass()))
            .map(auth -> (HTTPTokenAuthEntry) auth)
            .findFirst()
            .get();

        Assertions.assertNotNull(tokenAuth);
        Assertions.assertEquals("http://localhost:5555/oauth/token", tokenAuth.getTokenUrl());
        Assertions.assertEquals("http-request-client-id", tokenAuth.getClientId());
        Assertions.assertEquals("http-request-client-secret", tokenAuth.getClientSecret());
        Assertions.assertEquals("test", tokenAuth.getScope());
    }

    @Test
    public void parseRequestNotFound() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.not-found.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(CommandException.class,
            () -> parser.parseInput(settings, requestPath));
    }

    @Test
    @SuppressWarnings("unchecked")
    public void parseLoadDefaults() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.defaults.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        RequestInput<HTTPRequestEntry> requestInput = Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));

        HTTPRequestEntry request = requestInput.getRequests().get("request");

        Assertions.assertNotNull(request);

        List<String> values = (List<String>) request.getDefaults().get("steps");
        int defaultIndex = Assertions.assertDoesNotThrow(
            () -> Integer.parseInt((String) request.getDefaults().get(getDefaultFieldIndex("steps"))));

        Assertions.assertEquals("value1", request.getDefaults().get("field1"));
        Assertions.assertEquals("value2", request.getDefaults().get("field2"));
        Assertions.assertEquals("value3", request.getDefaults().get("field3"));
        Assertions.assertEquals("1/4", values.get(0));
        Assertions.assertEquals("2/4", values.get(1));
        Assertions.assertEquals("3/4", values.get(2));
        Assertions.assertEquals("4/4", values.get(3));
        Assertions.assertEquals("3/4", values.get(defaultIndex));
    }

    @Test
    public void parseFieldNotFound() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.field-not-found.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(CommandException.class,
            () -> parser.parseInput(settings, requestPath));
    }

    @Test
    public void parseInvalidFieldType() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.invalid-field-type.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(IllegalStateException.class,
            () -> parser.parseInput(settings, requestPath));
    }

    @Test
    public void parseRequestURL() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.url.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        RequestInput<HTTPRequestEntry> requestInput = Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));

        HTTPRequestEntry request = requestInput.getRequests().get("request");

        Assertions.assertNotNull(request);
        Assertions.assertEquals("http://localhost:5555/spec", request.getUrl());
    }

    @Test
    public void parseRepeatedSections() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.repeated-sections.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(CommandException.class,
            () -> parser.parseInput(settings, requestPath));
    }

    @Test
    public void parseRequestWihtoutName() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.empty-name.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(CommandException.class,
            () -> parser.parseInput(settings, requestPath));
    }

    @Test
    public void parseFlow() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-flow.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        RequestInput<HTTPRequestEntry> requestInput = Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));

        FlowEntry flow = requestInput.getFlows().get("flow");

        Assertions.assertNotNull(flow);

        StepEntry step = flow.getSteps().get(0);

        Assertions.assertEquals("spec.http", step.getRequestInputPath());
        Assertions.assertEquals(1, step.getOptions().size());
        Assertions.assertEquals("--set", step.getOptions().get(0).getLeft().name());
    }

    @Test
    public void parseFlowWihtoutName() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-flow.empty-name.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(CommandException.class,
            () -> parser.parseInput(settings, requestPath));
    }

    @Test
    public void parseFlowEmptyStep() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-flow.empty-step.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));
    }

    @Test
    public void parseRequestByPath() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.txt");
        HTTPRequestParser parser = new HTTPRequestParser();

        HTTPRequestEntry request = Assertions.assertDoesNotThrow(
            () -> parser.parseRequest(settings, requestPath));

        Assertions.assertEquals("POST", request.getMethod());
        Assertions.assertEquals("http://localhost:5555/spec", request.getUrl());
        Assertions.assertEquals(1, request.getQueryParams().size());
        Assertions.assertEquals("value", request.getQueryParams().get("param"));
        Assertions.assertEquals(1, request.getConditions().size());
        Assertions.assertEquals("EQUALS_TO", request.getConditions().get(0).getName());
        Assertions.assertEquals(1, request.getHeaders().size());
        Assertions.assertEquals("application/json", request.getHeaders().get("Content-Type"));
        Assertions.assertNotNull(request.getBodyContent());
        Assertions.assertEquals(1, request.getOutputMappings().size());
        Assertions.assertEquals("{{HTTP/header.Resource-ID}}", request.getOutputMappings().get("basic.functions.id"));
        Assertions.assertEquals(2, request.getAssertions().size());
        Assertions.assertEquals("EQUALS_TO", request.getAssertions().get(0).getName());
        Assertions.assertEquals(EqualsToAssertionFunction.class.getName(), request.getAssertions().get(1).getAssertionClass());
        Assertions.assertEquals(2, request.getOptions().size());
        Assertions.assertEquals("--set", request.getOptions().get(0).getLeft().name());
        Assertions.assertEquals("--set", request.getOptions().get(1).getLeft().name());

        HTTPMockEntry mock = request.getMockDefinition();

        Assertions.assertNotNull(mock);
        Assertions.assertEquals("201", mock.getStatusCode());
        Assertions.assertEquals("{}", mock.getResponseContent());
        Assertions.assertEquals(1, mock.getResponseHeaders().size());
        Assertions.assertEquals("application/json", mock.getResponseHeaders().get("Accept"));

        HTTPTokenAuthEntry tokenAuth = request.getAuthEntries()
            .stream()
            .filter(auth -> HTTPTokenAuthEntry.class.isAssignableFrom(auth.getClass()))
            .map(auth -> (HTTPTokenAuthEntry) auth)
            .findFirst()
            .get();

        Assertions.assertNotNull(tokenAuth);
        Assertions.assertEquals("http://localhost:5555/oauth/token", tokenAuth.getTokenUrl());
        Assertions.assertEquals("http-request-client-id", tokenAuth.getClientId());
        Assertions.assertEquals("http-request-client-secret", tokenAuth.getClientSecret());
        Assertions.assertEquals("test", tokenAuth.getScope());
    }

    @Test
    public void parseAPIByPathLoadDefaults() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.config.txt");
        HTTPRequestParser parser = new HTTPRequestParser();

        HTTPRequestEntry api = Assertions.assertDoesNotThrow(
            () -> parser.parseRequest(settings, requestPath));

        Assertions.assertEquals("value1", api.getDefaults().get("field1"));
        Assertions.assertEquals("value2", api.getDefaults().get("field2"));
        Assertions.assertEquals("value3", api.getDefaults().get("field3"));
    }

    @Test
    public void parseRequestByPathFieldNotFound() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.field-not-found.txt");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(CommandException.class,
            () -> parser.parseRequest(settings, requestPath));
    }

    @Test
    public void parseRequestByPathInvalidFieldType() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.invalid-field-type.txt");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(IllegalStateException.class,
            () -> parser.parseRequest(settings, requestPath));
    }

    @Test
    public void parseRequestByEscapedCommentSign() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.comment-sign.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        RequestInput<HTTPRequestEntry> requestInput = Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));
        HTTPRequestEntry request = requestInput.getRequests().get("request");
        String expectedBody = "# This is a normal line"
            + "\n# Another line"
            + "\n\\# It doesn't required ESCAPED generator \\";
        StringExpander stringExpander = new StringExpander(settings);

        Assertions.assertNotNull(request);
        Assertions.assertEquals("POST", request.getMethod());
        Assertions.assertEquals("http://localhost:5555/spec", request.getUrl());
        Assertions.assertEquals(1, request.getQueryParams().size());
        Assertions.assertEquals("value", request.getQueryParams().get("param"));
        Assertions.assertEquals(1, request.getHeaders().size());
        Assertions.assertEquals("application/txt", request.getHeaders().get("Content-Type"));
        Assertions.assertEquals(expectedBody, stringExpander.replaceAllInContent(request.getBodyContent()));
    }

    @Test
    public void parseRequestByFileMissingPath() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.file-missing-path.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(IllegalStateException.class,
            () -> parser.parseInput(settings, requestPath));
    }

    @Test
    public void parseRequestByFileMissingField() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.file-missing-field.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(IllegalStateException.class,
            () -> parser.parseInput(settings, requestPath));
    }

    @Test
    public void parseRequestByFileNotFound() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.file-not-found.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(IllegalStateException.class,
            () -> parser.parseInput(settings, requestPath));
    }

    @Test
    public void parseRequestByHeaderWithMethodAsPrefix() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.headers.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        RequestInput<HTTPRequestEntry> requestInput = Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));
        HTTPRequestEntry request = requestInput.getRequests().get("headers");

        Assertions.assertEquals(10, request.getHeaders().size());
        Assertions.assertEquals("get", request.getHeaders().get("getHeader"));
        Assertions.assertEquals("head", request.getHeaders().get("headHeader"));
        Assertions.assertEquals("post", request.getHeaders().get("postHeader"));
        Assertions.assertEquals("put", request.getHeaders().get("putHeader"));
        Assertions.assertEquals("delete", request.getHeaders().get("deleteHeader"));
        Assertions.assertEquals("connect", request.getHeaders().get("connectHeader"));
        Assertions.assertEquals("options", request.getHeaders().get("optionsHeader"));
        Assertions.assertEquals("trace", request.getHeaders().get("traceHeader"));
        Assertions.assertEquals("patch", request.getHeaders().get("patchHeader"));
        Assertions.assertEquals("common", request.getHeaders().get("Common-Header"));
    }

    @Test
    public void parseRequestByEscapedLabeledEnvs() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.labeled-envs.http");
        HTTPRequestParser parser = new HTTPRequestParser();
        StringExpander stringExpander = new StringExpander(settings);

        RequestInput<HTTPRequestEntry> requestInput = Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));
        HTTPRequestEntry request = requestInput.getRequests().get("request");
        String expectedBody = "Composing request with envs"
            + "\n: This is a normal line"
            + "\nEnv labeled with regex"
            + "\n"
            + "\nEOF\n";

        Assertions.assertNotNull(request);
        Assertions.assertEquals("POST", request.getMethod());
        Assertions.assertEquals("http://localhost:5555/spec", request.getUrl());
        Assertions.assertEquals(1, request.getHeaders().size());
        Assertions.assertNull(request.getHeaders().get("Discarded-Header"));
        Assertions.assertEquals("application/txt", request.getHeaders().get("Content-Type"));
        Assertions.assertEquals(expectedBody, stringExpander.replaceAllInContent(request.getBodyContent()));
    }
}
