package com.legadi.cli.jurl.model.http;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class HTTPResponseEntryTest {

    @Test
    public void setterGetterValidation() {
        HTTPResponseEntry model = new HTTPResponseEntry();
        List<Path> sendFiles = new ArrayList<>();

        sendFiles.add(Paths.get("src/test/resources/file.csv"));
        model.setBodyPath(Paths.get("src/test/resources/basic-functions.body.json"));
        model.setSentFilePaths(sendFiles);
        model.setResponsePath(Paths.get("./executions/src/test/resources/basic-functions_spec_http/create/2024-02-23/2024-02-23.21-31-39.331000000.response"));
        model.setStatusCode(201);

        Map<String, String> responseHeaders = new HashMap<>();
        responseHeaders.put("Content-Type", "application/json");
        model.setResponseHeaders(responseHeaders);

        Assertions.assertEquals(Paths.get("src/test/resources/basic-functions.body.json"), model.getBodyPath());
        Assertions.assertEquals(Paths.get("src/test/resources/file.csv"), model.getSentFilePaths().get(0));
        Assertions.assertEquals(Paths.get("./executions/src/test/resources/basic-functions_spec_http/create/2024-02-23/2024-02-23.21-31-39.331000000.response"), model.getResponsePath());
        Assertions.assertEquals(201, model.getStatusCode());
        Assertions.assertEquals(1, model.getResponseHeaders().size());
        Assertions.assertEquals("application/json", model.getResponseHeaders().get("Content-Type"));
    }
}
