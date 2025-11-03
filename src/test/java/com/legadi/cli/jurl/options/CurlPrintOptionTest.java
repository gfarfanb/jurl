package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_REQUEST_BEHAVIOUR;
import static com.legadi.cli.jurl.embedded.util.ObjectName.ASSERTIONS_RESULT;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;
import static com.legadi.cli.jurl.model.RequestBehaviour.CURL_ONLY;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class CurlPrintOptionTest extends OptionAbstractTest<CurlPrintOption> {

    public CurlPrintOptionTest() {
        super("--curl", false);
    }

    @Test
    public void printCurlValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-c",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);
        Optional<AssertionResult> assertionResult = requestCatcher.getLast(correlationId, ASSERTIONS_RESULT);
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, RESPONSE);

        Assertions.assertEquals(CURL_ONLY.name(), settings.get(PROP_REQUEST_BEHAVIOUR));
        Assertions.assertFalse(assertionResult.isPresent());
        Assertions.assertTrue(response.getCurlCommand().startsWith("curl -X POST"));
        Assertions.assertEquals("http://localhost:" + port + "/basic/body", response.getRequestUrl());
        Assertions.assertTrue(response.getResponseHeaders().isEmpty());
        Assertions.assertNull(response.getResponsePath());
        Assertions.assertNull(response.getResult());
        Assertions.assertNull(response.getSentFilePaths());
        Assertions.assertEquals(0, response.getStatusCode());
    }
}
