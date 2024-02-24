package com.legadi.jurl;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.embedded.DummyAPITest;

public class JurlApplicationTest extends DummyAPITest {

    @Test
    public void mainValidation() {
        String[] args = { "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Assertions.assertDoesNotThrow(() -> JurlApplication.main(args));
    }

    @Test
    public void mainSkip() {
        String[] args = { "-h" };

        Assertions.assertDoesNotThrow(() -> JurlApplication.main(args));
    }

    @Test
    public void mainNoRequestInputFile() {
        String[] args = { "-n", "create" };

        Assertions.assertDoesNotThrow(() -> JurlApplication.main(args));
    }

    @Test
    public void mainUnexpectedError() {
        String[] args = { "-n", "create", "src/test/resources/basic-functions.spec.http" };

        requestCatcher.add(correlationId, "request-with-exception", "create");
        requestCatcher.add(correlationId, "request-with-exception-throw", new RuntimeException("Unexpected"));

        Assertions.assertDoesNotThrow(() -> JurlApplication.main(args));
    }

    @Test
    public void mainAsObject() {
        Assertions.assertDoesNotThrow(() -> new JurlApplication());
    }
}
