package com.legadi.jurl.executor;

import org.junit.jupiter.api.Test;

import com.legadi.jurl.embedded.EmbeddedAPITest;

public class RequestCommandTest extends EmbeddedAPITest {

    @Test
    public void post() {
        jurl("-n", "post", "src/test/resources/basic-functions.json");
    }
}
