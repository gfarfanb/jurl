package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_REQUEST_BEHAVIOUR;
import static com.legadi.jurl.model.RequestBehaviour.CURL_ONLY;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class CurlPrintOptionTest extends OptionTest<CurlPrintOption> {

    public CurlPrintOptionTest() {
        super("--curl");
    }

    @Test
    public void printCurlValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-c",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.get(correlationId, "settings");
        Optional<AssertionResult> assertionResult = requestCatcher.get(correlationId, "assertion-result");
        HTTPResponseEntry response = requestCatcher.get(correlationId, "response");

        Assertions.assertEquals(CURL_ONLY.name(), settings.get(PROP_REQUEST_BEHAVIOUR));
        Assertions.assertFalse(assertionResult.isPresent());
        Assertions.assertTrue(response.getCurlCommand().startsWith("curl -X POST"));
        Assertions.assertEquals("http://localhost:" + port + "/basic/body", response.getRequestUrl());
        Assertions.assertTrue(response.getResponseHeaders().isEmpty());
        Assertions.assertNull(response.getResponsePath());
        Assertions.assertNull(response.getResult());
        Assertions.assertNull(response.getSentFilePath());
        Assertions.assertEquals(0, response.getStatusCode());
    }
}
