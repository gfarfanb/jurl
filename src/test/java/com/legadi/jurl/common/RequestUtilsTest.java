package com.legadi.jurl.common;

import static com.legadi.jurl.common.RequestUtils.mergeRequestHeader;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.model.http.HTTPRequestEntry;

public class RequestUtilsTest {

    @Test
    public void mergeRequestHeaderValidation() {
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setUrl("https://localhost:9876");
        api.setProtocol("https");
        api.setDomain("localhost:9876");
        api.setPort(9876);
        api.setBasePath("/api");
        api.setEndpoint("/v1");

        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setUrl("http://localhost:1234");
        request.setProtocol("http");
        request.setDomain("localhost:1234");
        request.setPort(1234);
        request.setBasePath("/base");
        request.setEndpoint("/endpoint");

        mergeRequestHeader(api, request);

        Assertions.assertEquals("http://localhost:1234", request.getUrl());
        Assertions.assertEquals("http", request.getProtocol());
        Assertions.assertEquals("localhost:1234", request.getDomain());
        Assertions.assertEquals(1234, request.getPort());
        Assertions.assertEquals("/base", request.getBasePath());
        Assertions.assertEquals("/endpoint", request.getEndpoint());
    }

    @Test
    public void mergeRequestHeaderAPIValues() {
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setUrl("https://localhost:9876");
        api.setProtocol("https");
        api.setDomain("localhost:9876");
        api.setPort(9876);
        api.setBasePath("/api");
        api.setEndpoint("/v1");

        HTTPRequestEntry request = new HTTPRequestEntry();

        mergeRequestHeader(api, request);

        Assertions.assertEquals("https://localhost:9876", request.getUrl());
        Assertions.assertEquals("https", request.getProtocol());
        Assertions.assertEquals("localhost:9876", request.getDomain());
        Assertions.assertEquals(9876, request.getPort());
        Assertions.assertEquals("/api", request.getBasePath());
        Assertions.assertEquals("/v1", request.getEndpoint());
    }
}
