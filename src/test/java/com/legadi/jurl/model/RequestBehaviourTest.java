package com.legadi.jurl.model;

import static com.legadi.jurl.model.RequestBehaviour.valueOf;
import static com.legadi.jurl.model.RequestBehaviour.CURL_ONLY;
import static com.legadi.jurl.model.RequestBehaviour.PRINT_ONLY;
import static com.legadi.jurl.model.RequestBehaviour.REQUEST;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class RequestBehaviourTest {

    @Test
    public void valueOfValidation() {
        Assertions.assertEquals(CURL_ONLY, valueOf("CURL_ONLY"));
        Assertions.assertEquals(PRINT_ONLY, valueOf("PRINT_ONLY"));
        Assertions.assertEquals(REQUEST, valueOf("REQUEST"));
        Assertions.assertThrows(IllegalArgumentException.class,
            () -> valueOf("UNKNOWN"));
    }
}
