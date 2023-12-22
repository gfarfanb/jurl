package com.legadi.jurl.executor.http;

import static com.legadi.jurl.common.JsonUtils.jsonToObject;
import static com.legadi.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.jurl.common.SettingsConstants.PROP_REQUEST_BEHAVIOUR;
import static com.legadi.jurl.common.SettingsConstants.PROP_SKIP_CONDITIONS;
import static com.legadi.jurl.common.SettingsConstants.PROP_MOCK_REQUEST;
import static com.legadi.jurl.executor.http.HTTPRequestModifier.BODY_TEMPORAL_PATH;
import static com.legadi.jurl.model.AssertionType.CONDITION;
import static com.legadi.jurl.model.AuthorizationType.BASIC;
import static com.legadi.jurl.model.AuthorizationType.EMPTY;
import static com.legadi.jurl.model.AuthorizationType.TOKEN;
import static com.legadi.jurl.model.RequestBehaviour.CURL_ONLY;

import java.io.IOException;
import java.util.Base64;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.EmbeddedAPITest;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.executor.RequestExecutor;
import com.legadi.jurl.model.AssertionEntry;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.http.HTTPMockEntry;
import com.legadi.jurl.model.http.HTTPRequestAuthEntry;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class HTTPRequestExecutorTest extends EmbeddedAPITest {

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
    public void acceptConditionsPrintOption() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        settings.putOverride(PROP_REQUEST_BEHAVIOUR, CURL_ONLY.name());

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

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(404, response.getStatusCode());
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
        request.setBodyContent("{}");

        settings.putOverride("username", "test");
        settings.putOverride("password", "73st");

        HTTPRequestAuthEntry auth = new HTTPRequestAuthEntry();
        auth.setAuthType(BASIC.name());
        auth.setUsernameParam("username");
        auth.setPasswordParam("password");
        request.setRequestAuth(auth);

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

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");
        request.getHeaders().put("Content-Type", "application/json");
        request.setBodyContent("{}");

        String token = UUID.randomUUID().toString();

        settings.putOverride("token", token);
        

        HTTPRequestAuthEntry auth = new HTTPRequestAuthEntry();
        auth.setAuthType(TOKEN.name());
        auth.setTokenParam("token");
        request.setRequestAuth(auth);

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(201, response.getStatusCode());
        Assertions.assertTrue(response.getCurlCommand().contains("Authorization: Bearer " + token));
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
        request.setBodyContent("{}");

        String token = UUID.randomUUID().toString();

        settings.putOverride("token", token);
        

        HTTPRequestAuthEntry auth = new HTTPRequestAuthEntry();
        auth.setAuthType(EMPTY.name());
        request.setRequestAuth(auth);

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(201, response.getStatusCode());
        Assertions.assertFalse(response.getCurlCommand().contains("Authorization:"));
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
    public void executeRequestBasicPostUnableToConnect() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("basic-post");
        request.setUrl("http://localhost:" + port + "/basic/body");
        request.setMethod("POST");

        HTTPMockEntry mock = new HTTPMockEntry();
        mock.setExceptionClassOnConnect(IOException.class.getName());
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

        HTTPRequestFileEntry file = new HTTPRequestFileEntry();
        file.setName("uploaded.csv");
        file.setField("file");
        file.setPath("src/test/resources/file.csv");
        file.setMineType("text/csv");
        file.getFormData().put("identifier", UUID.randomUUID().toString());
        request.setRequestFile(file);

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

        HTTPRequestFileEntry file = new HTTPRequestFileEntry();
        file.setField("file");
        file.setPath("src/test/resources/file.csv");
        request.setRequestFile(file);

        HTTPResponseEntry response = Assertions.assertDoesNotThrow(
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));

        Assertions.assertEquals(201, response.getStatusCode());
    }

    @Test
    public void executeRequestUploadFileMissingPath() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("upload");
        request.setUrl("http://localhost:" + port + "/file");

        HTTPRequestFileEntry file = new HTTPRequestFileEntry();
        request.setRequestFile(file);

        Assertions.assertThrows(RequestException.class,
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
    }

    @Test
    public void executeRequestUploadFileMissingField() {
        HTTPRequestExecutor executor = findByNameOrFail(RequestExecutor.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        request.setName("upload");
        request.setUrl("http://localhost:" + port + "/file");

        HTTPRequestFileEntry file = new HTTPRequestFileEntry();
        
        request.setRequestFile(file);

        Assertions.assertThrows(RequestException.class,
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
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
        request.setRequestFile(file);

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
        request.setRequestFile(file);

        HTTPMockEntry mock = new HTTPMockEntry();
        mock.setExceptionClassOnConnect(IOException.class.getName());
        request.setMockDefinition(mock);

        settings.putOverride(PROP_MOCK_REQUEST, Boolean.TRUE.toString());
        settings.putOverride(BODY_TEMPORAL_PATH, "src/test/resources/basic-functions.body.json");

        Assertions.assertThrows(IllegalStateException.class,
            () -> executor.executeRequest(settings, "src/test/resources/http-request-executor.http", request));
    }
}
