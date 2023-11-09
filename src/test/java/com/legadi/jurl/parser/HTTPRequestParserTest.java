package com.legadi.jurl.parser;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.model.RequestInput;
import com.legadi.jurl.model.http.HTTPRequestEntry;

public class HTTPRequestParserTest {

    @Test
    public void parseInputAPI() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/http-request.api.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        RequestInput<HTTPRequestEntry> requestInput = Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));

        Assertions.assertNotNull(requestInput.getApi());
        Assertions.assertEquals("http", requestInput.getApi().getProtocol());
        Assertions.assertEquals("localhost", requestInput.getApi().getDomain());
        Assertions.assertEquals("8080", requestInput.getApi().getPort());
        Assertions.assertEquals("basic", requestInput.getApi().getBasePath());
        Assertions.assertEquals("application/json", requestInput.getApi().getHeaders().get("Content-Type"));
    }

}
