package com.legadi.jurl.executor;

import java.nio.charset.StandardCharsets;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.EmbeddedAPITest;
import com.legadi.jurl.embedded.model.BasicFunctionsEntity;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class RequestCommandTest extends EmbeddedAPITest {

    @Test
    public void crud() {
        UUID postIdentifier = jurl("-n", "post", "src/test/resources/basic-functions.json");
        HTTPRequestEntry postRequest = requestCatcher.get(new TypeToken<HTTPRequestEntry>() {}, postIdentifier);
        Settings postSettings = requestCatcher.get(new TypeToken<Settings>() {}, postIdentifier);

        Assertions.assertEquals("src/test/resources/basic-functions.json", postRequest.getRequestPath());
        Assertions.assertEquals("post", postRequest.getName());
        Assertions.assertNull(postRequest.getUrl());
        Assertions.assertEquals("http", postRequest.getProtocol());
        Assertions.assertEquals("localhost", postRequest.getDomain());
        Assertions.assertEquals(port, postRequest.getPort());
        Assertions.assertEquals("basic", postRequest.getBasePath());
        Assertions.assertEquals("/body", postRequest.getEndpoint());
        Assertions.assertEquals("POST", postRequest.getMethod());
        Assertions.assertTrue(postRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(1, postRequest.getHeaders().size());
        Assertions.assertEquals(StandardCharsets.UTF_8.name(), postRequest.getBodyCharset());
        Assertions.assertFalse(postRequest.getBodyContent().isEmpty());
        Assertions.assertNull(postRequest.getBodyFilePath());
        Assertions.assertNull(postRequest.getRequestFile());
        Assertions.assertEquals(1, postRequest.getOutputMappings().size());
        Assertions.assertEquals(1, postRequest.getAssertions().size());

        HTTPResponseEntry postResponse = requestCatcher.get(new TypeToken<HTTPResponseEntry>() {}, postIdentifier);
        BasicFunctionsEntity postEntity = requestCatcher.getLastSaved(new TypeToken<BasicFunctionsEntity>() {});
        Assertions.assertEquals("http://localhost:" + port + "/basic/body", postResponse.getRequestUrl());
        Assertions.assertTrue(postResponse.getCurlCommand().contains("-X POST"));
        Assertions.assertTrue(postResponse.getCurlCommand().contains("-H \"Content-Type: application/json\""));
        Assertions.assertTrue(postResponse.getCurlCommand().contains("http://localhost:" + port + "/basic/body"));
        Assertions.assertTrue(postResponse.getCurlCommand().contains("--data-raw"));
        Assertions.assertTrue(postResponse.getCurlCommand().contains(postEntity.getAccess().toString()));
        Assertions.assertTrue(postResponse.getCurlCommand().contains(postEntity.getName()));
        Assertions.assertTrue(postResponse.getCurlCommand().contains(postEntity.getEmail()));
        Assertions.assertTrue(postResponse.getCurlCommand().contains(postEntity.getNickname()));
        Assertions.assertTrue(postResponse.getCurlCommand().contains(postEntity.getAmount().toString()));
        Assertions.assertTrue(postResponse.getCurlCommand().contains(Boolean.toString(postEntity.isActive())));
        Assertions.assertTrue(postResponse.getCurlCommand().contains(Integer.toString(postEntity.getCoins())));
        Assertions.assertTrue(postResponse.getCurlCommand().contains(postEntity.getBio()));
        Assertions.assertTrue(postResponse.getCurlCommand().contains(postEntity.getType()));
        Assertions.assertEquals("HTTP/1.1 201", postResponse.getResult());
        Assertions.assertEquals("./executions/basic-functions_json/post/" + postSettings.getExecutionTag() + ".response",
            postResponse.getResponsePath().toString());
        Assertions.assertEquals(201, postResponse.getStatusCode());
        Assertions.assertEquals(6, postResponse.getResponseHeaders().size());

        jurl("-n", "get", "src/test/resources/basic-functions.json");
        jurl("-n", "update", "src/test/resources/basic-functions.json");
        jurl("-n", "delete", "src/test/resources/basic-functions.json");
    }
}
