package com.legadi.jurl.exception;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.model.http.HTTPRequestEntry;

public class RequestExceptionTest {

    @Test
    public void requestExceptionNoName() {
        try {
            throw new RequestException(new HTTPRequestEntry(), null);
        } catch(RuntimeException ex) {
            Assertions.assertDoesNotThrow(() -> (RequestException) ex);
            Assertions.assertTrue(ex.getMessage().isEmpty());
        }
    }

    @Test
    public void requestExceptionNamed() {
        HTTPRequestEntry request = new HTTPRequestEntry();
        request.setName("request");

        try {
            throw new RequestException(request, "Invalid request");
        } catch(RuntimeException ex) {
            String expected = "[request] - Invalid request";

            Assertions.assertDoesNotThrow(() -> (RequestException) ex);
            Assertions.assertEquals(expected, ex.getMessage());
        }
    }
}
