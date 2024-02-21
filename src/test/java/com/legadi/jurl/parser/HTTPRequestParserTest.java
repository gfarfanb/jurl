package com.legadi.jurl.parser;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.assertions.EqualsToAssertionFunction;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.RequestInput;
import com.legadi.jurl.model.StepEntry;
import com.legadi.jurl.model.http.HTTPMockEntry;
import com.legadi.jurl.model.http.HTTPRequestAuthEntry;
import com.legadi.jurl.model.http.HTTPRequestEntry;

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

        HTTPRequestAuthEntry auth = api.getRequestAuth();

        Assertions.assertNotNull(auth);
        Assertions.assertEquals("TOKEN", auth.getAuthType());
        Assertions.assertEquals("auth.bearer.token", auth.getTokenParam());
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

        Assertions.assertNotNull(request);
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

        HTTPRequestAuthEntry auth = request.getRequestAuth();

        Assertions.assertNotNull(auth);
        Assertions.assertEquals("TOKEN", auth.getAuthType());
        Assertions.assertEquals("auth.bearer.token", auth.getTokenParam());
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
    public void parseLoadConfig() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.config.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        RequestInput<HTTPRequestEntry> requestInput = Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));

        HTTPRequestEntry request = requestInput.getRequests().get("request");

        Assertions.assertNotNull(request);

        Assertions.assertEquals("value1", settings.get("field1"));
        Assertions.assertEquals("value2", settings.get("field2"));
        Assertions.assertEquals("value3", settings.get("field3"));
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

        List<StepEntry> steps = requestInput.getFlows().get("flow");

        Assertions.assertNotNull(steps);

        StepEntry step = steps.get(0);

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

        HTTPRequestAuthEntry auth = request.getRequestAuth();

        Assertions.assertNotNull(auth);
        Assertions.assertEquals("TOKEN", auth.getAuthType());
        Assertions.assertEquals("auth.bearer.token", auth.getTokenParam());
    }

    @Test
    public void parseRequestByPathLoadConfig() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.config.txt");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertDoesNotThrow(
            () -> parser.parseRequest(settings, requestPath));

        Assertions.assertEquals("value1", settings.get("field1"));
        Assertions.assertEquals("value2", settings.get("field2"));
        Assertions.assertEquals("value3", settings.get("field3"));
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
}
