package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.common.JsonUtils.jsonToObject;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_MOCK_REQUEST;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_REQUEST_BEHAVIOUR;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_CONDITIONS;
import static com.legadi.cli.jurl.common.WriterUtils.writeFile;
import static com.legadi.cli.jurl.executor.http.HTTPRequestModifier.BODY_TEMPORAL_PATH;
import static com.legadi.cli.jurl.model.AssertionType.CONDITION;
import static com.legadi.cli.jurl.model.RequestBehaviour.CURL_ONLY;
import static com.legadi.cli.jurl.model.RequestBehaviour.PRINT_ONLY;

import java.io.IOException;
import java.nio.file.Path;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Base64;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.ObjectsRegistry;
import com.legadi.cli.jurl.common.OutputPathBuilder;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.EmbeddedAPIAbstractTest;
import com.legadi.cli.jurl.embedded.executor.HTTPRequestTestExecutor;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.exception.RequestException;
import com.legadi.cli.jurl.executor.RequestExecutor;
import com.legadi.cli.jurl.model.AssertionEntry;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.http.HTTPMockEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPBasicAuthEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;

public class HTTPRequestExecutorTest extends EmbeddedAPIAbstractTest {

    @BeforeEach
    public void setup() {
        ObjectsRegistry.register(RequestExecutor.class, 
            HTTPRequestTestExecutor.class, UUID.randomUUID(), requestCatcher);
    }

    @Test
    public void requestTypeValidation() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = Assertions.assertDoesNotThrow(
            () -> jsonToObject("{}", executor.requestType()));

        Assertions.assertNotNull(request);
    }

    @Test
    public void acceptConditionsValidation() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        AssertionEntry condition = new AssertionEntry();
        condition.setType(CONDITION);
        condition.setArgs(new String[] { "a", "a" });
        condition.setName("==");
        request.getConditions().add(condition);

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> executor.acceptsConditions(settings, request));

        Assertions.assertTrue(result.isPresent());
        Assertions.assertTrue(result.get().isPassed());
    }

    @Test
    public void acceptConditionsSkipConditions() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        settings.putOverride(PROP_SKIP_CONDITIONS, Boolean.TRUE.toString());

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> executor.acceptsConditions(settings, request));

        Assertions.assertFalse(result.isPresent());
    }

    @Test
    public void acceptConditionsCurlOnly() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        settings.putOverride(PROP_REQUEST_BEHAVIOUR, CURL_ONLY.name());

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> executor.acceptsConditions(settings, request));

        Assertions.assertFalse(result.isPresent());
    }

    @Test
    public void acceptConditionsPrintOnly() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        settings.putOverride(PROP_REQUEST_BEHAVIOUR, PRINT_ONLY.name());

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> executor.acceptsConditions(settings, request));

        Assertions.assertFalse(result.isPresent());
    }

    @Test
    public void executeRequestBasicPost() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.getHeaders().put("Request-Catcher", requestCatcherId);
        request.setBodyContent("{}");

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(201, response.getStatusCode());
    }

    @Test
    public void executeRequestBasicPostBodyFile() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.getHeaders().put("Request-Catcher", requestCatcherId);
        request.setBodyFilePath("src/test/resources/basic-functions.body.json");

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(201, response.getStatusCode());
    }

    @Test
    public void executeRequestBasicPostBodyTemporal() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.getHeaders().put("Request-Catcher", requestCatcherId);

        settings.putOverride(BODY_TEMPORAL_PATH, "src/test/resources/basic-functions.body.json");

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(201, response.getStatusCode());
    }

    @Test
    public void executeRequestBasicPostNoBody() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(400, response.getStatusCode());
    }

    @Test
    public void executeRequestBasicGet() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-get");
        request.setUrl("http://localhost:" + port + "/basic/body/" + UUID.randomUUID());
        request.setMethod("GET");
        request.getHeaders().put("Request-Catcher", requestCatcherId);

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(404, response.getStatusCode());
    }

    @Test
    public void executeRequestBasicEmpty() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-empty");
        request.setUrl("http://localhost:" + port + "/basic/body/empty");
        request.setMethod("GET");

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(200, response.getStatusCode());
    }

    @Test
    public void executeRequestBasicAuth() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.getHeaders().put("Request-Catcher", requestCatcherId);
        request.setBodyContent("{}");

        settings.putOverride("username", "test");
        settings.putOverride("password", "73st");

        HTTPBasicAuthEntry auth = new HTTPBasicAuthEntry();
        auth.setUsername("test");
        auth.setPassword("73st");
        request.setBasicAuth(auth);

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        String basicValue = Base64.getEncoder().encodeToString("test:73st".getBytes());

        Assertions.assertEquals(201, response.getStatusCode());
        Assertions.assertTrue(response.getCurlCommand().contains("Authorization: Basic " + basicValue));
    }

    @Test
    public void executeRequestBearerToken() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();
        HTTPAuthEntryFactory authEntryFactory = new HTTPAuthEntryFactory(settings);

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.getHeaders().put("Request-Catcher", requestCatcherId);
        request.setBodyContent("{}");

        HTTPTokenAuthEntry auth = authEntryFactory.instanceTokenAuth();
        auth.setTokenUrl("http://localhost:" + port + "/oauth/token");
        auth.setClientId(UUID.randomUUID().toString());
        auth.setClientSecret(UUID.randomUUID().toString());
        auth.setScope("test");
        request.setTokenAuth(auth);

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(201, response.getStatusCode());
    }

    @Test
    public void executeRequestAuthEmpty() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.getHeaders().put("Request-Catcher", requestCatcherId);
        request.setBodyContent("{}");

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(201, response.getStatusCode());
        Assertions.assertFalse(response.getCurlCommand().contains("Authorization:"));
    }

    @Test
    public void executeRequestCurlOnly() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.getHeaders().put("Request-Catcher", requestCatcherId);
        request.setBodyContent("{}");

        settings.putOverride(PROP_REQUEST_BEHAVIOUR, CURL_ONLY.name());

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(0, response.getStatusCode());
        Assertions.assertTrue(response.getResponseHeaders().isEmpty());
        Assertions.assertEquals("curl -X POST "
            + "-H \"Request-Catcher: " + requestCatcherId + "\" "
            + "-H \"Content-Type: application/json\" "
            + "--data-binary \"@./executions/src/test/resources/http-request-executor_http/basic-post/" + settings.getTimestamp().toLocalDate() + "/" + settings.getExecutionTag() + ".body\" "
            + "\"http://localhost:" + port + "/basic/body\"", response.getCurlCommand());
    }

    @Test
    public void executeRequestPrintOnly() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.setBodyContent("{}");

        settings.putOverride(PROP_REQUEST_BEHAVIOUR, PRINT_ONLY.name());

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(0, response.getStatusCode());
        Assertions.assertTrue(response.getResponseHeaders().isEmpty());
        Assertions.assertTrue(response.getCurlCommand().contains("curl -X POST"));
        Assertions.assertTrue(response.getCurlCommand().contains("-H \"Content-Type: application/json\""));
        Assertions.assertTrue(response.getCurlCommand().contains("--data-binary \"@./executions/src/test/resources/http-request-executor_http/basic-post/" + settings.getTimestamp().toLocalDate() + "/" + settings.getExecutionTag() + ".body\""));
        Assertions.assertTrue(response.getCurlCommand().contains("\"http://localhost:" + port + "/basic/body\""));
    }

    @Test
    public void executeRequestMalformedURL() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setUrl("not-a-url");
        request.setMethod("POST");

        Assertions.assertThrows(RequestException.class,
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
    }

    @Test
    public void executeRequestMethodNotDefined() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setUrl("http://localhost:" + port + "/basic/body");

        Assertions.assertThrows(RequestException.class,
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
    }

    @Test
    public void executeRequestInvalidMethod() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("LOCAL");

        Assertions.assertThrows(RequestException.class,
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
    }

    @Test
    public void executeRequestBasicPostUnableToSend() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");

        HTTPMockEntry mock = new HTTPMockEntry();
        mock.setExceptionClassOnOutputStream(IOException.class.getName());
        request.setMockDefinition(mock);

        settings.putOverride(PROP_MOCK_REQUEST, Boolean.TRUE.toString());
        settings.putOverride(BODY_TEMPORAL_PATH, "src/test/resources/basic-functions.body.json");

        Assertions.assertThrows(IllegalStateException.class,
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
    }

    @Test
    public void executeRequestBasicPostErrorResponseCode() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.setBodyContent("{}");

        HTTPMockEntry mock = new HTTPMockEntry();
        mock.setExceptionClassOnResponseCode(IOException.class.getName());
        request.setMockDefinition(mock);

        settings.putOverride(PROP_MOCK_REQUEST, Boolean.TRUE.toString());
        settings.putOverride(BODY_TEMPORAL_PATH, "src/test/resources/basic-functions.body.json");

        Assertions.assertThrows(IllegalStateException.class,
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
    }

    @Test
    public void executeRequestUploadFile() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("upload");
        request.setUrl("http://localhost:" + port + "/file");
        request.getHeaders().put("Request-Catcher", requestCatcherId);
        request.getFormData().put("identifier", UUID.randomUUID().toString());

        HTTPRequestFileEntry file = new HTTPRequestFileEntry();
        file.setName("uploaded.csv");
        file.setField("file");
        file.setPath("src/test/resources/file.csv");
        file.setMineType("text/csv");
        request.getRequestFiles().add(file);

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(201, response.getStatusCode());
    }

    @Test
    public void executeRequestUploadFileWithDefaults() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("upload");
        request.setUrl("http://localhost:" + port + "/file");
        request.getHeaders().put("Request-Catcher", requestCatcherId);

        HTTPRequestFileEntry file = new HTTPRequestFileEntry();
        file.setField("file");
        file.setPath("src/test/resources/file.csv");
        request.getRequestFiles().add(file);

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(201, response.getStatusCode());
    }

    @Test
    public void executeRequestUploadFileInvalidMethod() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("upload");
        request.setUrl("http://localhost:" + port + "/file");
        request.setMethod("LOCAL");

        HTTPRequestFileEntry file = new HTTPRequestFileEntry();
        request.getRequestFiles().add(file);

        Assertions.assertThrows(RequestException.class,
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
    }

    @Test
    public void executeRequestUploadFileUnableToConnect() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("upload");
        request.setUrl("http://localhost:" + port + "/file");
        request.setMethod("POST");

        HTTPRequestFileEntry file = new HTTPRequestFileEntry();
        file.setField("file");
        file.setPath("src/test/resources/file.csv");
        request.getRequestFiles().add(file);

        HTTPMockEntry mock = new HTTPMockEntry();
        mock.setExceptionClassOnOutputStream(IOException.class.getName());
        request.setMockDefinition(mock);

        settings.putOverride(PROP_MOCK_REQUEST, Boolean.TRUE.toString());
        settings.putOverride(BODY_TEMPORAL_PATH, "src/test/resources/basic-functions.body.json");

        Assertions.assertThrows(IllegalStateException.class,
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
    }

    @Test
    public void executeRequestDownloadFile() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("download");
        request.setUrl("http://localhost:" + port + "/file");
        request.setMethod("GET");
        request.getQueryParams().put("file", "src/test/resources/file.csv");

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(200, response.getStatusCode());
    }

    @Test
    public void executeRequestDownloadNamedFile() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("download");
        request.setUrl("http://localhost:" + port + "/file");
        request.setMethod("GET");
        request.getQueryParams().put("file", "src/test/resources/file.csv");
        request.getQueryParams().put("name", "downloaded.csv");

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(200, response.getStatusCode());
        Assertions.assertTrue(response.getResponsePath().toString().endsWith("downloaded.csv"));
    }

    @Test
    public void executeRequestMockDefinition() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-mock");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.setBodyContent("{}");

        HTTPMockEntry mock = new HTTPMockEntry();
        mock.setStatusCode("201");
        mock.setSecondsDelay("0");
        mock.getResponseHeaders().put("Keep-Alive", "timeout=60");
        mock.getResponseHeaders().put("Connection", "keep-alive");
        mock.getResponseHeaders().put("Content-Length", "7");
        mock.getResponseHeaders().put("Date", DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
        mock.getResponseHeaders().put("Content-Type", "text/html;charset=UTF-8");
        mock.setResponseContent("Created");
        request.setMockDefinition(mock);

        settings.putOverride(PROP_MOCK_REQUEST, Boolean.TRUE.toString());

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(201, response.getStatusCode());
    }

    @Test
    public void executeRequestMockNoDefinition() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-mock");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.setBodyContent("{}");

        settings.putOverride(PROP_MOCK_REQUEST, Boolean.TRUE.toString());

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(0, response.getStatusCode());
    }

    @Test
    public void executeRequestMockWithResponseFile() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-mock");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.setBodyContent("{}");

        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
                .setRequestPath("src/test/resources/http-request-executor.http")
                .setRequestName(request.getName())
                .setExtension("response");
        Path responsePath = pathBuilder.buildCommandPath();

        writeFile(responsePath, "Created");

        HTTPMockEntry mock = new HTTPMockEntry();
        mock.setStatusCode("201");
        mock.setResponseFilePath(responsePath.toString());
        request.setMockDefinition(mock);

        settings.putOverride(PROP_MOCK_REQUEST, Boolean.TRUE.toString());

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(201, response.getStatusCode());
    }

    @Test
    public void executeRequestMockWithDelay() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-mock");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.setBodyContent("{}");

        HTTPMockEntry mock = new HTTPMockEntry();
        mock.setStatusCode("201");
        mock.setSecondsDelay("1");
        request.setMockDefinition(mock);

        settings.putOverride(PROP_MOCK_REQUEST, Boolean.TRUE.toString());

        long beginTime = System.nanoTime();
        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
        Long endTime = System.nanoTime();

        Assertions.assertEquals(201, response.getStatusCode());
        Assertions.assertTrue(endTime - beginTime >= 1000000000L);
    }

    @Test
    public void executeRequestMockWrongStatusCode() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-mock");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.setBodyContent("{}");

        HTTPMockEntry mock = new HTTPMockEntry();
        mock.setStatusCode("CREATED");
        request.setMockDefinition(mock);

        settings.putOverride(PROP_MOCK_REQUEST, Boolean.TRUE.toString());

        Assertions.assertThrows(CommandException.class,
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
    }

    @Test
    public void executeRequestMockWrongSecondsDelay() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-mock");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.setBodyContent("{}");

        HTTPMockEntry mock = new HTTPMockEntry();
        mock.setStatusCode("201");
        mock.setSecondsDelay("LATE");
        request.setMockDefinition(mock);

        settings.putOverride(PROP_MOCK_REQUEST, Boolean.TRUE.toString());

        Assertions.assertThrows(CommandException.class,
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
    }
}
