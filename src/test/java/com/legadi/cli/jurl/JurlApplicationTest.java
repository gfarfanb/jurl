package com.legadi.cli.jurl;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.embedded.DummyAPIAbstractTest;

public class JurlApplicationTest extends DummyAPIAbstractTest {

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
