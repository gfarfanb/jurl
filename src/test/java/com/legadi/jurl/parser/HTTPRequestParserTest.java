package com.legadi.jurl.parser;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.RequestInput;
import com.legadi.jurl.model.http.HTTPRequestEntry;

public class HTTPRequestParserTest {

    @Test
    public void typeValidation() {
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertEquals("http", parser.type());
    }

    @Test
    public void parseAPI() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.api.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        RequestInput<HTTPRequestEntry> requestInput = Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));

        Assertions.assertNotNull(requestInput.getApi());
        Assertions.assertEquals("http", requestInput.getApi().getProtocol());
        Assertions.assertEquals("localhost", requestInput.getApi().getHost());
        Assertions.assertEquals("8080", requestInput.getApi().getPort());
        Assertions.assertEquals("basic", requestInput.getApi().getBasePath());
        Assertions.assertEquals("application/json", requestInput.getApi().getHeaders().get("Content-Type"));
    }

    @Test
    public void parseWithoutSections() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.no-sections.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        Assertions.assertThrows(CommandException.class,
            () -> parser.parseInput(settings, requestPath));
    }

    @Test
    public void parseRequest() {
        Settings settings = new Settings();
        Path requestPath = Paths.get("src/test/resources/parser/http-request.spec.http");
        HTTPRequestParser parser = new HTTPRequestParser();

        RequestInput<HTTPRequestEntry> requestInput = Assertions.assertDoesNotThrow(
            () -> parser.parseInput(settings, requestPath));
    }
}
