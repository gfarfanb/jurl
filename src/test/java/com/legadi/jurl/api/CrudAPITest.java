package com.legadi.jurl.api;

import static com.legadi.jurl.executor.http.HTTPRequestExecutor.BODY_TEMPORAL_PATH;
import static com.legadi.jurl.common.JsonUtils.loadJsonFile;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.EmbeddedAPITest;
import com.legadi.jurl.embedded.model.BasicFunctionsEntity;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class CrudAPITest extends EmbeddedAPITest {

    @Test
    public void crud() {
        UUID id = post();
        get(id);
        put(id);
        delete(id);
    }

    private UUID post() {
        UUID createIdentifier = jurl("-n", "create", "src/test/resources/basic-functions.spec.http");
        String requestInputPath = requestCatcher.get(new TypeToken<String>() {}, createIdentifier);
        HTTPRequestEntry createRequest = requestCatcher.get(new TypeToken<HTTPRequestEntry>() {}, createIdentifier);

        Assertions.assertEquals("src/test/resources/basic-functions.spec.http", requestInputPath);
        Assertions.assertEquals("create", createRequest.getName());
        Assertions.assertNull(createRequest.getUrl());
        Assertions.assertEquals("http", createRequest.getProtocol());
        Assertions.assertEquals("localhost", createRequest.getDomain());
        Assertions.assertEquals(Integer.toString(port), createRequest.getPort());
        Assertions.assertEquals("basic", createRequest.getBasePath());
        Assertions.assertEquals("/body", createRequest.getEndpoint());
        Assertions.assertEquals("POST", createRequest.getMethod());
        Assertions.assertTrue(createRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(1, createRequest.getHeaders().size());
        Assertions.assertEquals(StandardCharsets.UTF_8.name(), createRequest.getBodyCharset());
        Assertions.assertFalse(createRequest.getBodyContent().isEmpty());
        Assertions.assertNull(createRequest.getBodyFilePath());
        Assertions.assertNull(createRequest.getRequestFile());
        Assertions.assertEquals(1, createRequest.getOutputMappings().size());
        Assertions.assertEquals(1, createRequest.getAssertions().size());

        HTTPResponseEntry createResponse = requestCatcher.get(new TypeToken<HTTPResponseEntry>() {}, createIdentifier);
        BasicFunctionsEntity createEntity = requestCatcher.getLastSaved(new TypeToken<BasicFunctionsEntity>() {}).getRight();
        Settings createSettings = requestCatcher.get(new TypeToken<Settings>() {}, createIdentifier);

        Assertions.assertEquals("http://localhost:" + port + "/basic/body", createResponse.getRequestUrl());
        Assertions.assertTrue(createResponse.getCurlCommand().contains("-X POST"));
        Assertions.assertTrue(createResponse.getCurlCommand().contains("-H \"Content-Type: application/json\""));
        Assertions.assertTrue(createResponse.getCurlCommand().contains("http://localhost:" + port + "/basic/body"));
        Assertions.assertTrue(createResponse.getCurlCommand().contains("--data-raw"));
        Assertions.assertTrue(createResponse.getCurlCommand().contains(createEntity.getAccess().toString()));
        Assertions.assertTrue(createResponse.getCurlCommand().contains(createEntity.getName()));
        Assertions.assertTrue(createResponse.getCurlCommand().contains(createEntity.getEmail()));
        Assertions.assertTrue(createResponse.getCurlCommand().contains(createEntity.getNickname()));
        Assertions.assertTrue(createResponse.getCurlCommand().contains(createEntity.getAmount().toString()));
        Assertions.assertTrue(createResponse.getCurlCommand().contains(Boolean.toString(createEntity.isActive())));
        Assertions.assertTrue(createResponse.getCurlCommand().contains(Integer.toString(createEntity.getCoins())));
        Assertions.assertTrue(createResponse.getCurlCommand().contains(createEntity.getBio()));
        Assertions.assertTrue(createResponse.getCurlCommand().contains(createEntity.getType()));
        Assertions.assertTrue(createResponse.getCurlCommand().contains(DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(createEntity.getTimestamp())));
        Assertions.assertEquals("HTTP/1.1 201", createResponse.getResult());
        Assertions.assertEquals("./executions/src/test/resources/basic-functions_spec_http/create/" + createSettings.getExecutionTag() + ".response",
            createResponse.getResponsePath().toString());
        Assertions.assertEquals(201, createResponse.getStatusCode());
        Assertions.assertEquals(6, createResponse.getResponseHeaders().size());

        Optional<AssertionResult> createAssertionResult = requestCatcher.get(new TypeToken<Optional<AssertionResult>>() {}, createIdentifier);

        Assertions.assertTrue(createAssertionResult.isPresent());
        Assertions.assertEquals(1, createAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, createAssertionResult.get().getFailures());
        Assertions.assertFalse(createAssertionResult.get().isSkip());

        return UUID.fromString(createSettings.get("basic.functions.id"));
    }

    private void get(UUID id) {
        UUID obtainIdentifier = jurl("-n", "obtain", "src/test/resources/basic-functions.spec.http");
        String requestInputPath = requestCatcher.get(new TypeToken<String>() {}, obtainIdentifier);
        HTTPRequestEntry obtainRequest = requestCatcher.get(new TypeToken<HTTPRequestEntry>() {}, obtainIdentifier);

        Assertions.assertEquals("src/test/resources/basic-functions.spec.http", requestInputPath);
        Assertions.assertEquals("obtain", obtainRequest.getName());
        Assertions.assertNull(obtainRequest.getUrl());
        Assertions.assertEquals("http", obtainRequest.getProtocol());
        Assertions.assertEquals("localhost", obtainRequest.getDomain());
        Assertions.assertEquals(Integer.toString(port), obtainRequest.getPort());
        Assertions.assertEquals("basic", obtainRequest.getBasePath());
        Assertions.assertEquals("/body/" + id, obtainRequest.getEndpoint());
        Assertions.assertEquals("GET", obtainRequest.getMethod());
        Assertions.assertTrue(obtainRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(1, obtainRequest.getHeaders().size());
        Assertions.assertEquals(StandardCharsets.UTF_8.name(), obtainRequest.getBodyCharset());
        Assertions.assertNull(obtainRequest.getBodyContent());
        Assertions.assertNull(obtainRequest.getBodyFilePath());
        Assertions.assertNull(obtainRequest.getRequestFile());
        Assertions.assertEquals(1, obtainRequest.getOutputMappings().size());
        Assertions.assertEquals(1, obtainRequest.getAssertions().size());

        HTTPResponseEntry obtainResponse = requestCatcher.get(new TypeToken<HTTPResponseEntry>() {}, obtainIdentifier);
        Settings obtainSettings = requestCatcher.get(new TypeToken<Settings>() {}, obtainIdentifier);

        Assertions.assertEquals("http://localhost:" + port + "/basic/body/" + id, obtainResponse.getRequestUrl());
        Assertions.assertTrue(obtainResponse.getCurlCommand().contains("-X GET"));
        Assertions.assertTrue(obtainResponse.getCurlCommand().contains("-H \"Content-Type: application/json\""));
        Assertions.assertTrue(obtainResponse.getCurlCommand().contains("http://localhost:" + port + "/basic/body/" + id));
        Assertions.assertEquals("HTTP/1.1 200", obtainResponse.getResult());
        Assertions.assertEquals("./executions/src/test/resources/basic-functions_spec_http/obtain/" + obtainSettings.getExecutionTag() + ".response",
            obtainResponse.getResponsePath().toString());
        Assertions.assertEquals(200, obtainResponse.getStatusCode());
        Assertions.assertEquals(5, obtainResponse.getResponseHeaders().size());

        BasicFunctionsEntity obtainEntity = requestCatcher.get(new TypeToken<BasicFunctionsEntity>() {}, id);
        Map<String, Object> obtainInputBody = loadJsonFile(obtainSettings.get("basic.functions.entity"), new TypeToken<Map<String, Object>>() {});

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

        Optional<AssertionResult> obtainAssertionResult = requestCatcher.get(new TypeToken<Optional<AssertionResult>>() {}, obtainIdentifier);
        Assertions.assertTrue(obtainAssertionResult.isPresent());
        Assertions.assertEquals(1, obtainAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, obtainAssertionResult.get().getFailures());
        Assertions.assertFalse(obtainAssertionResult.get().isSkip());
    }

    private void put(UUID id) {
        UUID updateIdentifier = jurl("-n", "update", "-mb", "json", "src/test/resources/basic-functions.spec.http");
        Settings updateSettings = requestCatcher.get(new TypeToken<Settings>() {}, updateIdentifier);
        String requestInputPath = requestCatcher.get(new TypeToken<String>() {}, updateIdentifier);
        HTTPRequestEntry updateRequest = requestCatcher.get(new TypeToken<HTTPRequestEntry>() {}, updateIdentifier);

        Assertions.assertEquals("src/test/resources/basic-functions.spec.http", requestInputPath);
        Assertions.assertEquals("update", updateRequest.getName());
        Assertions.assertNull(updateRequest.getUrl());
        Assertions.assertEquals("http", updateRequest.getProtocol());
        Assertions.assertEquals("localhost", updateRequest.getDomain());
        Assertions.assertEquals(Integer.toString(port), updateRequest.getPort());
        Assertions.assertEquals("basic", updateRequest.getBasePath());
        Assertions.assertEquals("/body/" + id, updateRequest.getEndpoint());
        Assertions.assertEquals("PUT", updateRequest.getMethod());
        Assertions.assertTrue(updateRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(1, updateRequest.getHeaders().size());
        Assertions.assertEquals(StandardCharsets.UTF_8.name(), updateRequest.getBodyCharset());
        Assertions.assertNull(updateRequest.getBodyContent());
        Assertions.assertNull(updateRequest.getBodyFilePath());
        Assertions.assertDoesNotThrow(() -> updateSettings.get(BODY_TEMPORAL_PATH));
        Assertions.assertNull(updateRequest.getRequestFile());
        Assertions.assertTrue(updateRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, updateRequest.getAssertions().size());

        HTTPResponseEntry updateResponse = requestCatcher.get(new TypeToken<HTTPResponseEntry>() {}, updateIdentifier);

        Assertions.assertEquals("http://localhost:" + port + "/basic/body/" + id, updateResponse.getRequestUrl());
        Assertions.assertTrue(updateResponse.getCurlCommand().contains("-X PUT"));
        Assertions.assertTrue(updateResponse.getCurlCommand().contains("-H \"Content-Type: application/json\""));
        Assertions.assertTrue(updateResponse.getCurlCommand().contains("http://localhost:" + port + "/basic/body/" + id));
        Assertions.assertTrue(updateResponse.getCurlCommand().contains("--data-binary"));
        Assertions.assertTrue(updateResponse.getCurlCommand().contains("@" + updateSettings.get(BODY_TEMPORAL_PATH)));
        Assertions.assertEquals("HTTP/1.1 204", updateResponse.getResult());
        Assertions.assertNull(updateResponse.getResponsePath());
        Assertions.assertEquals(204, updateResponse.getStatusCode());
        Assertions.assertEquals(3, updateResponse.getResponseHeaders().size());

        Optional<AssertionResult> updateAssertionResult = requestCatcher.get(new TypeToken<Optional<AssertionResult>>() {}, updateIdentifier);

        Assertions.assertTrue(updateAssertionResult.isPresent());
        Assertions.assertEquals(1, updateAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, updateAssertionResult.get().getFailures());
        Assertions.assertFalse(updateAssertionResult.get().isSkip());

        BasicFunctionsEntity updateEntity = requestCatcher.getLastSaved(new TypeToken<BasicFunctionsEntity>() {}).getRight();
        BasicFunctionsEntity updateBody = loadJsonFile(updateSettings.get(BODY_TEMPORAL_PATH), new TypeToken<BasicFunctionsEntity>() {});

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

    private void delete(UUID id) {
        UUID removeIdentifier = jurl("-n", "remove", "src/test/resources/basic-functions.spec.http");
        String requestInputPath = requestCatcher.get(new TypeToken<String>() {}, removeIdentifier);
        HTTPRequestEntry removeRequest = requestCatcher.get(new TypeToken<HTTPRequestEntry>() {}, removeIdentifier);

        Assertions.assertEquals("src/test/resources/basic-functions.spec.http", requestInputPath);
        Assertions.assertEquals("remove", removeRequest.getName());
        Assertions.assertNull(removeRequest.getUrl());
        Assertions.assertEquals("http", removeRequest.getProtocol());
        Assertions.assertEquals("localhost", removeRequest.getDomain());
        Assertions.assertEquals(Integer.toString(port), removeRequest.getPort());
        Assertions.assertEquals("basic", removeRequest.getBasePath());
        Assertions.assertEquals("/body/" + id, removeRequest.getEndpoint());
        Assertions.assertEquals("DELETE", removeRequest.getMethod());
        Assertions.assertTrue(removeRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(1, removeRequest.getHeaders().size());
        Assertions.assertEquals(StandardCharsets.UTF_8.name(), removeRequest.getBodyCharset());
        Assertions.assertNull(removeRequest.getBodyContent());
        Assertions.assertNull(removeRequest.getBodyFilePath());
        Assertions.assertNull(removeRequest.getRequestFile());
        Assertions.assertTrue(removeRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, removeRequest.getAssertions().size());

        HTTPResponseEntry removeResponse = requestCatcher.get(new TypeToken<HTTPResponseEntry>() {}, removeIdentifier);

        Assertions.assertEquals("http://localhost:" + port + "/basic/body/" + id, removeResponse.getRequestUrl());
        Assertions.assertTrue(removeResponse.getCurlCommand().contains("-X DELETE"));
        Assertions.assertTrue(removeResponse.getCurlCommand().contains("-H \"Content-Type: application/json\""));
        Assertions.assertTrue(removeResponse.getCurlCommand().contains("http://localhost:" + port + "/basic/body/" + id));
        Assertions.assertEquals("HTTP/1.1 204", removeResponse.getResult());
        Assertions.assertNull(removeResponse.getResponsePath());
        Assertions.assertEquals(204, removeResponse.getStatusCode());
        Assertions.assertEquals(3, removeResponse.getResponseHeaders().size());

        Optional<AssertionResult> removeAssertionResult = requestCatcher.get(new TypeToken<Optional<AssertionResult>>() {}, removeIdentifier);

        Assertions.assertTrue(removeAssertionResult.isPresent());
        Assertions.assertEquals(1, removeAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, removeAssertionResult.get().getFailures());
        Assertions.assertFalse(removeAssertionResult.get().isSkip());
    }
}
