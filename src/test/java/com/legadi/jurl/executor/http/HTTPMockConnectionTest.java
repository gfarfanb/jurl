package com.legadi.jurl.executor.http;

import static com.legadi.jurl.common.WriterUtils.writeFile;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Path;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.OutputPathBuilder;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.http.HTTPMockEntry;

public class HTTPMockConnectionTest {

    @Test
    @SuppressWarnings("resource")
    public void mockConnection() throws MalformedURLException {
        Settings settings = new Settings();
        URL local = new URL("http://localhost:0/base");
        HTTPMockConnection connection = new HTTPMockConnection(local, settings,
            "src/test/resources/http-mock-connection-test", "test", null);

        Assertions.assertEquals("http://localhost:0/base", connection.getURL().toString());
        Assertions.assertDoesNotThrow(() -> connection.disconnect());

        boolean usingProxy = Assertions.assertDoesNotThrow(() -> connection.usingProxy());
        Assertions.assertFalse(usingProxy);

        Assertions.assertDoesNotThrow(() -> connection.connect());
        Assertions.assertDoesNotThrow(() -> connection.setRequestMethod("POST"));
        Assertions.assertDoesNotThrow(() -> connection.setRequestProperty("property", "value"));
        Assertions.assertDoesNotThrow(() -> connection.setDoOutput(true));

        boolean doOutput = Assertions.assertDoesNotThrow(() -> connection.getDoOutput());
        Assertions.assertTrue(doOutput);

        OutputStream outputStream = Assertions.assertDoesNotThrow(() -> connection.getOutputStream());
        Assertions.assertNotNull(outputStream);

        Assertions.assertDoesNotThrow(() -> connection.setDoInput(true));
        Assertions.assertDoesNotThrow(() -> connection.setUseCaches(true));
    }

    @Test
    public void mockConnectionWithDefinition() throws IOException {
        Settings settings = new Settings();
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setStatusCode("201");
        mock.setSecondsDelay("0");
        mock.setResponseContent("Created");
        mock.getResponseHeaders().put("Keep-Alive", "timeout=60");
        mock.getResponseHeaders().put("Connection", "keep-alive");
        mock.getResponseHeaders().put("Content-Length", "7");
        mock.getResponseHeaders().put("Date", DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
        mock.getResponseHeaders().put("Content-Type", "text/html;charset=UTF-8");

        HTTPMockConnection connection = new HTTPMockConnection(null, settings,
            "src/test/resources/http-mock-connection-test", "test", mock);
        String responseContent = null;

        try(InputStreamReader inputStreamReader = new InputStreamReader(connection.getInputStream());
                BufferedReader bufferedReader = new BufferedReader(inputStreamReader)) {
            responseContent = bufferedReader.lines().collect(Collectors.joining());
        }

        Assertions.assertEquals("Created", responseContent);
        Assertions.assertEquals(201, connection.getResponseCode());
        Assertions.assertEquals(6, connection.getHeaderFields().size());
    }

    @Test
    public void mockConnectionWithResponseFile() throws IOException {
        Settings settings = new Settings();
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
                .setRequestPath("src/test/resources/http-mock-connection.http")
                .setRequestName("mock-connection")
                .setExtension("response");
        Path responsePath = pathBuilder.buildCommandPath();

        writeFile(responsePath, "Created");

        URL local = new URL("http://localhost:0/base");
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setResponseFilePath(responsePath.toString());

        HTTPMockConnection connection = new HTTPMockConnection(local, settings,
            "src/test/resources/http-mock-connection-test", "test", mock);
        String responseContent = null;

        try(InputStreamReader inputStreamReader = new InputStreamReader(connection.getInputStream());
                BufferedReader bufferedReader = new BufferedReader(inputStreamReader)) {
            responseContent = bufferedReader.lines().collect(Collectors.joining());
        }

        Assertions.assertEquals("Created", responseContent);
        Assertions.assertEquals(0, connection.getResponseCode());
    }

    @Test
    @SuppressWarnings("resource")
    public void mockConnectionWithDelay() throws MalformedURLException {
        Settings settings = new Settings();
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setStatusCode("201");
        mock.setSecondsDelay("1");

        HTTPMockConnection connection = new HTTPMockConnection(null, settings,
            "src/test/resources/http-mock-connection-test", "test", mock);

        long beginTime = System.nanoTime();
        Assertions.assertDoesNotThrow(() -> connection.getInputStream());
        Long endTime = System.nanoTime();

        Assertions.assertTrue(endTime - beginTime >= 1000000000L);
    }

    @Test
    public void mockConnectionWrongStatusCode() throws MalformedURLException {
        Settings settings = new Settings();
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setStatusCode("CREATED");

        Assertions.assertThrows(CommandException.class,
            () -> new HTTPMockConnection(null, settings,
                "src/test/resources/http-mock-connection-test", "test", mock));
    }

    @Test
    public void mockConnectionWrongSecondsDelay() throws MalformedURLException {
        Settings settings = new Settings();
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setStatusCode("201");
        mock.setSecondsDelay("LATE");

        Assertions.assertThrows(CommandException.class,
            () -> new HTTPMockConnection(null, settings,
                "src/test/resources/http-mock-connection-test", "test", mock));
    }

    @Test
    public void mockConnectionExceptionOnOutputStream() {
        Settings settings = new Settings();
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setExceptionClassOnOutputStream(IOException.class.getName());

        HTTPMockConnection connection = new HTTPMockConnection(null, settings,
            "src/test/resources/http-mock-connection-test", "test", mock);

        Assertions.assertThrows(IOException.class,
            () -> connection.getOutputStream());
    }

    @Test
    public void mockConnectionExceptionOnResponseCode() {
        Settings settings = new Settings();
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setExceptionClassOnResponseCode(IOException.class.getName());

        HTTPMockConnection connection = new HTTPMockConnection(null, settings,
            "src/test/resources/http-mock-connection-test", "test", mock);

        Assertions.assertThrows(IOException.class,
            () -> connection.getResponseCode());
    }

    @Test
    @SuppressWarnings("resource")
    public void mockConnectionWrongExceptionClass() {
        Settings settings = new Settings();
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setExceptionClassOnOutputStream(Object.class.getName());
        mock.setExceptionClassOnResponseCode(Object.class.getName());

        HTTPMockConnection connection = new HTTPMockConnection(null, settings,
            "src/test/resources/http-mock-connection-test", "test", mock);

        Assertions.assertDoesNotThrow(() -> connection.getOutputStream());
        Assertions.assertDoesNotThrow(() -> connection.getResponseCode());
    }
}
