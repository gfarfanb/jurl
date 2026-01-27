package com.legadi.cli.jurl.api;

import static com.legadi.cli.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.cli.jurl.embedded.util.FilenameUtils.toSystemSeparator;
import static com.legadi.cli.jurl.embedded.util.ObjectName.ASSERTIONS_RESULT;
import static com.legadi.cli.jurl.embedded.util.ObjectName.BODY;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST_INPUT_PATH;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;
import static com.legadi.cli.jurl.executor.http.HTTPRequestModifier.BODY_TEMPORAL_PATH;

import java.math.BigDecimal;
import java.nio.file.Paths;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.EmbeddedAPIAbstractTest;
import com.legadi.cli.jurl.embedded.model.BasicFunctionsEntity;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class CrudAPITest extends EmbeddedAPIAbstractTest {

    @Test
    public void post() {
        UUID createCorrelationId = jurl("-n", "create", "src/test/resources/basic-functions.spec.http");
        String requestInputPath = requestCatcher.getLast(createCorrelationId, REQUEST_INPUT_PATH);
        HTTPRequestEntry createRequest = requestCatcher.getLast(createCorrelationId, REQUEST);

        Assertions.assertEquals("src/test/resources/basic-functions.spec.http", requestInputPath);
        Assertions.assertEquals("create", createRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/basic/body", createRequest.getUrl());
        Assertions.assertEquals("POST", createRequest.getMethod());
        Assertions.assertTrue(createRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(2, createRequest.getHeaders().size());
        Assertions.assertNull(createRequest.getBodyCharset());
        Assertions.assertFalse(createRequest.getBodyContent().isEmpty());
        Assertions.assertEquals("src/test/resources/basic-functions.body.json", createRequest.getBodyFilePath());
        Assertions.assertTrue(createRequest.getRequestFiles().isEmpty());
        Assertions.assertTrue(createRequest.getFormData().isEmpty());
        Assertions.assertEquals(1, createRequest.getOutputMappings().size());
        Assertions.assertEquals(1, createRequest.getAssertions().size());

        HTTPResponseEntry createResponse = requestCatcher.getLast(createCorrelationId, RESPONSE);
        Settings createSettings = requestCatcher.getLast(createCorrelationId, SETTINGS);
        String expectedResponsePath = toSystemSeparator(
            "./executions/src/test/resources/basic-functions_spec_http/create/"
            + createSettings.getTimestamp().toLocalDate() + "/"
            + createSettings.getExecutionTag() + ".response"
        );

        Assertions.assertEquals("http://localhost:" + port + "/basic/body", createResponse.getRequestUrl());
        Assertions.assertTrue(createResponse.getCurlCommand().contains("-X POST"));
        Assertions.assertTrue(createResponse.getCurlCommand().contains("-H \"Content-Type: application/json\""));
        Assertions.assertTrue(createResponse.getCurlCommand().contains("http://localhost:" + port + "/basic/body"));
        Assertions.assertTrue(createResponse.getCurlCommand().contains("--data-binary"));
        Assertions.assertTrue(createResponse.getCurlCommand().contains("@" + createResponse.getBodyPath()));
        Assertions.assertEquals("HTTP/1.1 201", createResponse.getResult());
        Assertions.assertEquals(expectedResponsePath, createResponse.getResponsePath().toString());
        Assertions.assertEquals(201, createResponse.getStatusCode());
        Assertions.assertFalse(createResponse.getResponseHeaders().isEmpty());
        Assertions.assertTrue(createResponse.getResponseHeaders().containsKey("Resource-ID"));
        Assertions.assertDoesNotThrow(
            () -> UUID.fromString(createResponse.getResponseHeaders().get("Resource-ID")));

        Optional<AssertionResult> createAssertionResult = requestCatcher.getLast(createCorrelationId, ASSERTIONS_RESULT);

        Assertions.assertTrue(createAssertionResult.isPresent());
        Assertions.assertEquals(1, createAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, createAssertionResult.get().getFailures());
        Assertions.assertTrue(createAssertionResult.get().isPassed());
        Assertions.assertDoesNotThrow(
            () -> UUID.fromString(createSettings.get("basic.functions.id")));
    }

    @Test
    public void get() {
        UUID id = UUID.randomUUID();

        requestCatcher.add(id, BODY, loadJsonFile(Paths.get("src/test/resources/basic-functions.body.json"),
            new TypeToken<BasicFunctionsEntity>() {}, null));

        UUID obtainCorrelationId = jurl(
            "-s", "basic.functions.id", id.toString(),
            "-n", "obtain", "src/test/resources/basic-functions.spec.http");
        String requestInputPath = requestCatcher.getLast(obtainCorrelationId, REQUEST_INPUT_PATH);
        HTTPRequestEntry obtainRequest = requestCatcher.getLast(obtainCorrelationId, REQUEST);

        Assertions.assertEquals("src/test/resources/basic-functions.spec.http", requestInputPath);
        Assertions.assertEquals("obtain", obtainRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/basic/body/" + id, obtainRequest.getUrl());
        Assertions.assertEquals("GET", obtainRequest.getMethod());
        Assertions.assertTrue(obtainRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(2, obtainRequest.getHeaders().size());
        Assertions.assertNull(obtainRequest.getBodyCharset());
        Assertions.assertNull(obtainRequest.getBodyContent());
        Assertions.assertNull(obtainRequest.getBodyFilePath());
        Assertions.assertTrue(obtainRequest.getRequestFiles().isEmpty());
        Assertions.assertTrue(obtainRequest.getFormData().isEmpty());
        Assertions.assertEquals(1, obtainRequest.getOutputMappings().size());
        Assertions.assertEquals(1, obtainRequest.getAssertions().size());

        HTTPResponseEntry obtainResponse = requestCatcher.getLast(obtainCorrelationId, RESPONSE);
        Settings obtainSettings = requestCatcher.getLast(obtainCorrelationId, SETTINGS);
        String expectedResponsePath = toSystemSeparator(
            "./executions/src/test/resources/basic-functions_spec_http/obtain/"
            + obtainSettings.getTimestamp().toLocalDate() + "/"
            + obtainSettings.getExecutionTag() + ".response"
        );

        Assertions.assertEquals("http://localhost:" + port + "/basic/body/" + id, obtainResponse.getRequestUrl());
        Assertions.assertTrue(obtainResponse.getCurlCommand().contains("-X GET"));
        Assertions.assertTrue(obtainResponse.getCurlCommand().contains("-H \"Content-Type: application/json\""));
        Assertions.assertTrue(obtainResponse.getCurlCommand().contains("http://localhost:" + port + "/basic/body/" + id));
        Assertions.assertEquals("HTTP/1.1 200", obtainResponse.getResult());
        Assertions.assertEquals(expectedResponsePath, obtainResponse.getResponsePath().toString());
        Assertions.assertEquals(200, obtainResponse.getStatusCode());
        Assertions.assertFalse(obtainResponse.getResponseHeaders().isEmpty());
        Assertions.assertTrue(obtainResponse.getResponseHeaders().containsKey("Content-Type"));
        Assertions.assertEquals("application/json", obtainResponse.getResponseHeaders().get("Content-Type"));

        BasicFunctionsEntity obtainEntity = requestCatcher.getLast(id, BODY);
        Map<String, Object> obtainInputBody = loadJsonFile(obtainSettings.get("basic.functions.entity"), new TypeToken<Map<String, Object>>() {}, null);

        Assertions.assertEquals(obtainEntity.getAccess().toString(), obtainInputBody.get("access"));
        Assertions.assertEquals(obtainEntity.getName(), obtainInputBody.get("name"));
        Assertions.assertEquals(obtainEntity.getEmail(), obtainInputBody.get("email"));
        Assertions.assertEquals(obtainEntity.getNickname(), obtainInputBody.get("nickname"));
        Assertions.assertEquals(obtainEntity.getAmount(), obtainInputBody.get("amount"));
        Assertions.assertEquals(obtainEntity.isActive(), obtainInputBody.get("active"));
        Assertions.assertEquals(BigDecimal.valueOf(obtainEntity.getCoins()), obtainInputBody.get("coins"));
        Assertions.assertEquals(obtainEntity.getBio(), obtainInputBody.get("bio"));
        Assertions.assertEquals(obtainEntity.getType(), obtainInputBody.get("type"));
        Assertions.assertEquals(DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(obtainEntity.getTimestamp()), obtainInputBody.get("timestamp"));

        Optional<AssertionResult> obtainAssertionResult = requestCatcher.getLast(obtainCorrelationId, ASSERTIONS_RESULT);
        Assertions.assertTrue(obtainAssertionResult.isPresent());
        Assertions.assertEquals(1, obtainAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, obtainAssertionResult.get().getFailures());
        Assertions.assertTrue(obtainAssertionResult.get().isPassed());
    }

    @Test
    public void put() {
        UUID id = UUID.randomUUID();

        requestCatcher.add(id, BODY, loadJsonFile(Paths.get("src/test/resources/basic-functions.body.json"),
            new TypeToken<BasicFunctionsEntity>() {}, null));

        UUID updateCorrelationId = jurl(
            "-s", "basic.functions.id", id.toString(),
            "-n", "update", "-mb", "json", "src/test/resources/basic-functions.spec.http");
        Settings updateSettings = requestCatcher.getLast(updateCorrelationId, SETTINGS);
        String requestInputPath = requestCatcher.getLast(updateCorrelationId, REQUEST_INPUT_PATH);
        HTTPRequestEntry updateRequest = requestCatcher.getLast(updateCorrelationId, REQUEST);

        Assertions.assertEquals("src/test/resources/basic-functions.spec.http", requestInputPath);
        Assertions.assertEquals("update", updateRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/basic/body/" + id, updateRequest.getUrl());
        Assertions.assertEquals("PUT", updateRequest.getMethod());
        Assertions.assertTrue(updateRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(2, updateRequest.getHeaders().size());
        Assertions.assertNull(updateRequest.getBodyCharset());
        Assertions.assertNull(updateRequest.getBodyContent());
        Assertions.assertNull(updateRequest.getBodyFilePath());
        Assertions.assertDoesNotThrow(() -> updateSettings.get(BODY_TEMPORAL_PATH));
        Assertions.assertTrue(updateRequest.getRequestFiles().isEmpty());
        Assertions.assertTrue(updateRequest.getFormData().isEmpty());
        Assertions.assertTrue(updateRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, updateRequest.getAssertions().size());

        HTTPResponseEntry updateResponse = requestCatcher.getLast(updateCorrelationId, RESPONSE);

        Assertions.assertEquals("http://localhost:" + port + "/basic/body/" + id, updateResponse.getRequestUrl());
        Assertions.assertTrue(updateResponse.getCurlCommand().contains("-X PUT"));
        Assertions.assertTrue(updateResponse.getCurlCommand().contains("-H \"Content-Type: application/json\""));
        Assertions.assertTrue(updateResponse.getCurlCommand().contains("http://localhost:" + port + "/basic/body/" + id));
        Assertions.assertTrue(updateResponse.getCurlCommand().contains("--data-binary"));
        Assertions.assertTrue(updateResponse.getCurlCommand().contains("@" + updateSettings.get(BODY_TEMPORAL_PATH)));
        Assertions.assertEquals("HTTP/1.1 204", updateResponse.getResult());
        Assertions.assertNull(updateResponse.getResponsePath());
        Assertions.assertEquals(204, updateResponse.getStatusCode());
        Assertions.assertFalse(updateResponse.getResponseHeaders().isEmpty());

        Optional<AssertionResult> updateAssertionResult = requestCatcher.getLast(updateCorrelationId, ASSERTIONS_RESULT);

        Assertions.assertTrue(updateAssertionResult.isPresent());
        Assertions.assertEquals(1, updateAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, updateAssertionResult.get().getFailures());
        Assertions.assertTrue(updateAssertionResult.get().isPassed());

        BasicFunctionsEntity updateEntity = (BasicFunctionsEntity) requestCatcher
            .getLastSaved(BODY)
            .getRight();
        BasicFunctionsEntity updateBody = loadJsonFile(updateSettings.get(BODY_TEMPORAL_PATH), new TypeToken<BasicFunctionsEntity>() {}, null);

        Assertions.assertEquals(updateEntity.getAccess(), updateBody.getAccess());
        Assertions.assertEquals(updateEntity.getName(), updateBody.getName());
        Assertions.assertEquals("jurl@test.com", updateBody.getEmail());
        Assertions.assertEquals(updateEntity.getNickname(), updateBody.getNickname());
        Assertions.assertEquals(updateEntity.getAmount(), updateBody.getAmount());
        Assertions.assertEquals(updateEntity.isActive(), updateBody.isActive());
        Assertions.assertEquals(updateEntity.getCoins(), updateBody.getCoins());
        Assertions.assertEquals(updateEntity.getBio(), updateBody.getBio());
        Assertions.assertEquals(updateEntity.getType(), updateBody.getType());
        Assertions.assertEquals(updateEntity.getTimestamp(), updateBody.getTimestamp());
    }

    @Test
    public void delete() {
        UUID id = UUID.randomUUID();

        requestCatcher.add(id, BODY, loadJsonFile(Paths.get("src/test/resources/basic-functions.body.json"),
            new TypeToken<BasicFunctionsEntity>() {}, null));

        UUID removeCorrelationId = jurl(
            "-s", "basic.functions.id", id.toString(),
            "-n", "remove", "src/test/resources/basic-functions.spec.http");
        String requestInputPath = requestCatcher.getLast(removeCorrelationId, REQUEST_INPUT_PATH);
        HTTPRequestEntry removeRequest = requestCatcher.getLast(removeCorrelationId, REQUEST);

        Assertions.assertEquals("src/test/resources/basic-functions.spec.http", requestInputPath);
        Assertions.assertEquals("remove", removeRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/basic/body/" + id, removeRequest.getUrl());
        Assertions.assertEquals("DELETE", removeRequest.getMethod());
        Assertions.assertTrue(removeRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(2, removeRequest.getHeaders().size());
        Assertions.assertNull(removeRequest.getBodyCharset());
        Assertions.assertNull(removeRequest.getBodyContent());
        Assertions.assertNull(removeRequest.getBodyFilePath());
        Assertions.assertTrue(removeRequest.getRequestFiles().isEmpty());
        Assertions.assertTrue(removeRequest.getFormData().isEmpty());
        Assertions.assertTrue(removeRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, removeRequest.getAssertions().size());

        HTTPResponseEntry removeResponse = requestCatcher.getLast(removeCorrelationId, RESPONSE);

        Assertions.assertEquals("http://localhost:" + port + "/basic/body/" + id, removeResponse.getRequestUrl());
        Assertions.assertTrue(removeResponse.getCurlCommand().contains("-X DELETE"));
        Assertions.assertTrue(removeResponse.getCurlCommand().contains("-H \"Content-Type: application/json\""));
        Assertions.assertTrue(removeResponse.getCurlCommand().contains("http://localhost:" + port + "/basic/body/" + id));
        Assertions.assertEquals("HTTP/1.1 204", removeResponse.getResult());
        Assertions.assertNull(removeResponse.getResponsePath());
        Assertions.assertEquals(204, removeResponse.getStatusCode());
        Assertions.assertFalse(removeResponse.getResponseHeaders().isEmpty());

        Optional<AssertionResult> removeAssertionResult = requestCatcher.getLast(removeCorrelationId, ASSERTIONS_RESULT);

        Assertions.assertTrue(removeAssertionResult.isPresent());
        Assertions.assertEquals(1, removeAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, removeAssertionResult.get().getFailures());
        Assertions.assertTrue(removeAssertionResult.get().isPassed());
    }
}
