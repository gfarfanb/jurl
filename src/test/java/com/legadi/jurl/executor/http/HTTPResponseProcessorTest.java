package com.legadi.jurl.executor.http;

import static com.legadi.jurl.common.ObjectsRegistry.findByNameOrFail;

import org.junit.jupiter.api.Test;

import com.legadi.jurl.executor.ResponseProcessor;

public class HTTPResponseProcessorTest {

    @Test
    public void processResponseValidation() {
        HTTPResponseProcessor processor = findByNameOrFail(ResponseProcessor.class, "http");
        
    }
}
