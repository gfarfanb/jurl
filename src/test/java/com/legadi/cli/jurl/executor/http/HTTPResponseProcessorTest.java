package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_REQUEST_BEHAVIOUR;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_ASSERTIONS;
import static com.legadi.cli.jurl.embedded.util.FilenameUtils.toSystemSeparator;
import static com.legadi.cli.jurl.model.AssertionType.ASSERTION;
import static com.legadi.cli.jurl.model.RequestBehaviour.CURL_ONLY;
import static com.legadi.cli.jurl.model.RequestBehaviour.PRINT_ONLY;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.ObjectsRegistry;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.executor.HTTPResponseTestProcessor;
import com.legadi.cli.jurl.embedded.util.RequestCatcherManager;
import com.legadi.cli.jurl.executor.ResponseProcessor;
import com.legadi.cli.jurl.model.AssertionEntry;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class HTTPResponseProcessorTest {

    @BeforeEach
    public void setup() {
        ObjectsRegistry.register(ResponseProcessor.class,
            HTTPResponseTestProcessor.class, UUID.randomUUID(),
            RequestCatcherManager.getCatcher(UUID.randomUUID().toString()));
    }

    @Test
    public void processResponseValidation() {
        Settings settings = new Settings();
        HTTPRequestEntry request = new HTTPRequestEntry();
        AssertionEntry equals = new AssertionEntry();

        equals.setType(ASSERTION);
        equals.setName("EQUALS_TO");
        equals.setArgs(new String[] { "Successful", "{{OUT/status}}" });

        request.getAssertions().add(equals);
        request.getOutputMappings().put("output.status", "{{OUT/status}}");

        HTTPResponseEntry response = new HTTPResponseEntry();

        response.setStatusCode(200);
        response.setRequestUrl("http://localhost/path");
        response.setCurlCommand("curl http://localhost/path");
        response.setResponsePath(Paths.get("src/test/resources/json-response.output.gz.out"));
        response.getResponseHeaders().put("Content-Type", "application/json");
        response.getResponseHeaders().put("Accept", "application/json");

        HTTPResponseProcessor processor = findByNameOrFail(ResponseProcessor.class, "http");
        Optional<AssertionResult> assertionResult = Assertions.assertDoesNotThrow(
            () -> processor.process(settings, request, response));

        Assertions.assertTrue(assertionResult.isPresent());
        Assertions.assertEquals(1, assertionResult.get().getAssertions());
        Assertions.assertEquals(0, assertionResult.get().getFailures());
        Assertions.assertTrue(assertionResult.get().isPassed());

        String expectedResponsePath = toSystemSeparator(
            "src/test/resources/json-response.output.gz.out"
        );

        Assertions.assertEquals("http://localhost/path", settings.get("HTTP/url"));
        Assertions.assertEquals("curl http://localhost/path", settings.get("HTTP/curl"));
        Assertions.assertEquals("200", settings.get("HTTP/status"));
        Assertions.assertEquals(expectedResponsePath, settings.get("HTTP/response.path"));
        Assertions.assertEquals("application/json", settings.get("HTTP/header.Content-Type"));
    }

    @Test
    public void processResponseCurlOnly() {
        Settings settings = new Settings();
        HTTPResponseEntry response = new HTTPResponseEntry();

        settings.putOverride(PROP_REQUEST_BEHAVIOUR, CURL_ONLY.name());

        HTTPResponseProcessor processor = findByNameOrFail(ResponseProcessor.class, "http");
        Optional<AssertionResult> assertionResult = Assertions.assertDoesNotThrow(
            () -> processor.process(settings, null, response));

        Assertions.assertFalse(assertionResult.isPresent());
    }

    @Test
    public void processResponsePrintOnly() {
        Settings settings = new Settings();

        settings.putOverride(PROP_REQUEST_BEHAVIOUR, PRINT_ONLY.name());

        HTTPResponseProcessor processor = findByNameOrFail(ResponseProcessor.class, "http");
        Optional<AssertionResult> assertionResult = Assertions.assertDoesNotThrow(
            () -> processor.process(settings, null, null));

        Assertions.assertFalse(assertionResult.isPresent());
    }

    @Test
    public void processResponseSkipAssertions() {
        Settings settings = new Settings();

        settings.putOverride(PROP_SKIP_ASSERTIONS, Boolean.TRUE.toString());

        HTTPRequestEntry request = new HTTPRequestEntry();
        HTTPResponseEntry response = new HTTPResponseEntry();

        response.setStatusCode(200);
        response.setRequestUrl("http://localhost/path");
        response.setCurlCommand("curl http://localhost/path");
        response.setResponsePath(Paths.get("src/test/resources/json-response.output.gz.out"));
        response.getResponseHeaders().put("Content-Type", "application/json");

        HTTPResponseProcessor processor = findByNameOrFail(ResponseProcessor.class, "http");
        Optional<AssertionResult> assertionResult = Assertions.assertDoesNotThrow(
            () -> processor.process(settings, request, response));

        Assertions.assertFalse(assertionResult.isPresent());

        String expectedResponsePath = toSystemSeparator(
            "src/test/resources/json-response.output.gz.out"
        );

        Assertions.assertEquals("http://localhost/path", settings.get("HTTP/url"));
        Assertions.assertEquals("curl http://localhost/path", settings.get("HTTP/curl"));
        Assertions.assertEquals("200", settings.get("HTTP/status"));
        Assertions.assertEquals(expectedResponsePath, settings.get("HTTP/response.path"));
        Assertions.assertEquals("application/json", settings.get("HTTP/header.Content-Type"));
    }

    @Test
    public void processResponseNoResponse() {
        Settings settings = new Settings();
        HTTPRequestEntry request = new HTTPRequestEntry();
        HTTPResponseEntry response = new HTTPResponseEntry();

        HTTPResponseProcessor processor = findByNameOrFail(ResponseProcessor.class, "http");
        Optional<AssertionResult> assertionResult = Assertions.assertDoesNotThrow(
            () -> processor.process(settings, request, response));

        Assertions.assertFalse(assertionResult.isPresent());
    }

    @Test
    public void processResponseNoContentType() {
        Settings settings = new Settings();
        HTTPRequestEntry request = new HTTPRequestEntry();
        HTTPResponseEntry response = new HTTPResponseEntry();

        response.setResponsePath(Paths.get("src/test/resources/json-response.output.gz.out"));

        HTTPResponseProcessor processor = findByNameOrFail(ResponseProcessor.class, "http");
        Optional<AssertionResult> assertionResult = Assertions.assertDoesNotThrow(
            () -> processor.process(settings, request, response));

        Assertions.assertFalse(assertionResult.isPresent());
    }

    @Test
    public void processResponseGZipResponse() {
        Settings settings = new Settings();
        HTTPRequestEntry request = new HTTPRequestEntry();
        AssertionEntry equals = new AssertionEntry();

        equals.setType(ASSERTION);
        equals.setName("EQUALS_TO");
        equals.setArgs(new String[] { "Successful", "{{OUT/status}}" });

        request.getAssertions().add(equals);

        HTTPResponseEntry response = new HTTPResponseEntry();

        response.setResponsePath(Paths.get("src/test/resources/json-response.output.gz"));
        response.getResponseHeaders().put("Content-Encoding", "gzip");
        response.getResponseHeaders().put("Content-Type", "application/json");

        HTTPResponseProcessor processor = findByNameOrFail(ResponseProcessor.class, "http");
        Optional<AssertionResult> assertionResult = Assertions.assertDoesNotThrow(
            () -> processor.process(settings, request, response));

        Assertions.assertTrue(assertionResult.isPresent());
        Assertions.assertEquals(1, assertionResult.get().getAssertions());
        Assertions.assertEquals(0, assertionResult.get().getFailures());
        Assertions.assertTrue(assertionResult.get().isPassed());
    }

    @Test
    public void processResponseInvalidResponseFileType() {
        Settings settings = new Settings();
        HTTPRequestEntry request = new HTTPRequestEntry();
        HTTPResponseEntry response = new HTTPResponseEntry();

        response.setResponsePath(Paths.get("src/test/resources/json-response.output.gz"));
        response.getResponseHeaders().put("Content-Encoding", "compressed");
        response.getResponseHeaders().put("Content-Type", "application/json");

        HTTPResponseProcessor processor = findByNameOrFail(ResponseProcessor.class, "http");
        Optional<AssertionResult> assertionResult = Assertions.assertDoesNotThrow(
            () -> processor.process(settings, request, response));

        Assertions.assertFalse(assertionResult.isPresent());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void getDetailsFromResponseValidation() {
        HTTPResponseEntry response = new HTTPResponseEntry();
        List<Path> sendFiles = new ArrayList<>();

        sendFiles.add(Paths.get("src/test/resources/file.csv"));

        response.setBodyPath(Paths.get("src/test/resources/json-object.input.json"));
        response.setSentFilePaths(sendFiles);
        response.setResponsePath(Paths.get("src/test/resources/json-response.output.gz.out"));
        response.setStatusCode(200);
        response.getResponseHeaders().put("Content-Type", "application/json");

        HTTPResponseProcessor processor = findByNameOrFail(ResponseProcessor.class, "http");
        Map<String, Object> details = Assertions.assertDoesNotThrow(
            () -> processor.getDetails(response));
        String expectedBodyPath = toSystemSeparator(
            "src/test/resources/json-object.input.json"
        );
        String expectedSendFilePath = toSystemSeparator(
            "src/test/resources/file.csv"
        );
        String expectedResponsePath = toSystemSeparator(
            "src/test/resources/json-response.output.gz.out"
        );

        Assertions.assertEquals(expectedBodyPath, details.get("bodyPath"));
        Assertions.assertEquals(Arrays.asList(expectedSendFilePath), details.get("sentFilePaths"));
        Assertions.assertEquals(expectedResponsePath, details.get("responsePath"));
        Assertions.assertEquals(200, details.get("statusCode"));
        Assertions.assertNotNull(details.get("responseHeaders"));
        Assertions.assertEquals("application/json", ((Map<String, String>) details.get("responseHeaders")).get("Content-Type"));
    }

    @Test
    public void getDetailsFromResponseEmpty() {
        HTTPResponseEntry response = new HTTPResponseEntry();

        HTTPResponseProcessor processor = findByNameOrFail(ResponseProcessor.class, "http");
        Map<String, Object> details = Assertions.assertDoesNotThrow(
            () -> processor.getDetails(response));

        Assertions.assertNull(details.get("bodyPath"));
        Assertions.assertNull(details.get("sentFilePath"));
        Assertions.assertNull(details.get("responsePath"));
        Assertions.assertNull(details.get("statusCode"));
        Assertions.assertNull(details.get("responseHeaders"));
    }

    @Test
    public void getDetailsFromResponseNull() {
        HTTPResponseProcessor processor = findByNameOrFail(ResponseProcessor.class, "http");
        Map<String, Object> details = Assertions.assertDoesNotThrow(
            () -> processor.getDetails(null));

        Assertions.assertNull(details);
    }
}
