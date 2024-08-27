package com.legadi.cli.jurl.model;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class HistoryEntryTest {

    @Test
    public void setterGetterValidation() {
        HistoryEntry model = new HistoryEntry();
        String curl = "curl -X POST -H \"Request-Catcher: 9e78d43b-1e4b-4cd3-ba1e-72208906853e\" -H \"Content-Type: application/json\" --data-binary \"@./executions/src/test/resources/basic-functions_spec_http/create/2024-02-23/2024-02-23.21-31-39.331000000.body\" \"http://localhost:42121/basic/body\"";
        String bodyPath = "./executions/src/test/resources/basic-functions_spec_http/create/2024-02-23/2024-02-23.21-31-39.331000000.body";

        model.setCurl(curl);
        model.setResult("HTTP/1.1 201 OK");
        model.setRequestPath("src/test/resources/basic-functions.spec.http");
        model.setRequestName("create");
        model.setEnvironment("default");
        model.setWorkspacePath("/workspaces/jurl");
        model.setTimestamp(1708723899L);
        model.setExecutionTag("2024-02-23.21-31-39.331000000");
        model.setNanoTime(2050708L);
        model.setAssertions(2);
        model.setFailures(0);
        model.setIndex("1/1");

        Map<String, Object> details = new HashMap<>();
        details.put("bodyPath", bodyPath);
        model.setDetails(details);

        Assertions.assertEquals(curl, model.getCurl());
        Assertions.assertEquals("HTTP/1.1 201 OK", model.getResult());
        Assertions.assertEquals("src/test/resources/basic-functions.spec.http", model.getRequestPath());
        Assertions.assertEquals("create", model.getRequestName());
        Assertions.assertEquals("default", model.getEnvironment());
        Assertions.assertEquals("/workspaces/jurl", model.getWorkspacePath());
        Assertions.assertEquals(1708723899L, model.getTimestamp());
        Assertions.assertEquals("2024-02-23.21-31-39.331000000", model.getExecutionTag());
        Assertions.assertEquals(2050708L, model.getNanoTime());
        Assertions.assertEquals(2, model.getAssertions());
        Assertions.assertEquals(0, model.getFailures());
        Assertions.assertEquals("1/1", model.getIndex());
        Assertions.assertNotNull(model.getDetails());
        Assertions.assertEquals(1, model.getDetails().size());
        Assertions.assertEquals(bodyPath, model.getDetails().get("bodyPath"));
    }
}
