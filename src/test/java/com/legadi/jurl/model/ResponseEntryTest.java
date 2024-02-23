package com.legadi.jurl.model;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class ResponseEntryTest {

    @Test
    public void setterGetterValidation() {
        ResponseEntry model = new ResponseEntry();
        String curl = "curl -X POST -H \"Request-Catcher: 9e78d43b-1e4b-4cd3-ba1e-72208906853e\" -H \"Content-Type: application/json\" --data-binary \"@./executions/src/test/resources/basic-functions_spec_http/create/2024-02-23/2024-02-23.21-31-39.331000000.body\" \"http://localhost:42121/basic/body\"";

        model.setRequestUrl("http://localhost:42121/basic/body");
        model.setCurlCommand(curl);
        model.setResult("HTTP/1.1 201 OK");

        Assertions.assertEquals("http://localhost:42121/basic/body", model.getRequestUrl());
        Assertions.assertEquals(curl, model.getCurlCommand());
        Assertions.assertEquals("HTTP/1.1 201 OK", model.getResult());
    }
}
