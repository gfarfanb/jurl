package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.common.WriterUtils.writeFile;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.OutputPathBuilder;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.model.http.HTTPMockEntry;

public class HTTPMockConnectionTest {

    @Test
    public void mockConnection() throws MalformedURLException {
        Settings settings = new Settings();
        URL local = new URL("http://localhost:0/base");
        HTTPMockConnection connection = new HTTPMockConnection(local, settings,
            "src/test/resources/http-mock-connection-test", "test", new HashMap<>(), null);

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
            "src/test/resources/http-mock-connection-test", "test", new HashMap<>(), mock);
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
            "src/test/resources/http-mock-connection-test", "test", new HashMap<>(), mock);
        String responseContent = null;

        try(InputStreamReader inputStreamReader = new InputStreamReader(connection.getInputStream());
                BufferedReader bufferedReader = new BufferedReader(inputStreamReader)) {
            responseContent = bufferedReader.lines().collect(Collectors.joining());
        }

        Assertions.assertEquals("Created", responseContent);
        Assertions.assertEquals(0, connection.getResponseCode());
    }

    @Test
    public void mockConnectionWithDelay() throws MalformedURLException {
        Settings settings = new Settings();
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setStatusCode("201");
        mock.setSecondsDelay("1");

        HTTPMockConnection connection = new HTTPMockConnection(null, settings,
            "src/test/resources/http-mock-connection-test", "test", new HashMap<>(), mock);

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
                "src/test/resources/http-mock-connection-test", "test", new HashMap<>(), mock));
    }

    @Test
    public void mockConnectionWrongSecondsDelay() throws MalformedURLException {
        Settings settings = new Settings();
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setStatusCode("201");
        mock.setSecondsDelay("LATE");

        Assertions.assertThrows(CommandException.class,
            () -> new HTTPMockConnection(null, settings,
                "src/test/resources/http-mock-connection-test", "test", new HashMap<>(), mock));
    }

    @Test
    public void mockConnectionExceptionOnOutputStream() {
        Settings settings = new Settings();
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setExceptionClassOnOutputStream(IOException.class.getName());

        HTTPMockConnection connection = new HTTPMockConnection(null, settings,
            "src/test/resources/http-mock-connection-test", "test", new HashMap<>(), mock);

        Assertions.assertThrows(IOException.class,
            () -> connection.getOutputStream());
    }

    @Test
    public void mockConnectionExceptionOnResponseCode() {
        Settings settings = new Settings();
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setExceptionClassOnResponseCode(IOException.class.getName());

        HTTPMockConnection connection = new HTTPMockConnection(null, settings,
            "src/test/resources/http-mock-connection-test", "test", new HashMap<>(), mock);

        Assertions.assertThrows(IOException.class,
            () -> connection.getResponseCode());
    }

    @Test
    public void mockConnectionWrongExceptionClass() {
        Settings settings = new Settings();
        HTTPMockEntry mock = new HTTPMockEntry();

        mock.setExceptionClassOnOutputStream(Object.class.getName());
        mock.setExceptionClassOnResponseCode(Object.class.getName());

        HTTPMockConnection connection = new HTTPMockConnection(null, settings,
            "src/test/resources/http-mock-connection-test", "test", new HashMap<>(), mock);

        Assertions.assertDoesNotThrow(() -> connection.getOutputStream());
        Assertions.assertDoesNotThrow(() -> connection.getResponseCode());
    }

    @Test
    public void mockConnectionStringExpander() throws IOException {
        Settings settings = new Settings();

        settings.putUserInput("mockHeaderAccept", "application/json");
        settings.putUserInput("mockResponseContent", "{}");
        settings.putUserInput("mockResponseFilePath", "src/test/resources/mock-response.output.json");
        settings.putUserInput("mockExceptionClassOnOutputStream", IOException.class.getName());
        settings.putUserInput("mockExceptionClassOnResponseCode", IOException.class.getName());

        HTTPMockEntry mockContent = new HTTPMockEntry();
        mockContent.getResponseHeaders().put("Accept", "{{mockHeaderAccept}}");
        mockContent.setResponseContent("{{mockResponseContent}}");
        mockContent.setResponseFilePath("{{mockResponseFilePath}}");
        mockContent.setExceptionClassOnOutputStream("{{mockExceptionClassOnOutputStream}}");
        mockContent.setExceptionClassOnResponseCode("{{mockExceptionClassOnResponseCode}}");

        HTTPMockConnection connectionContent = new HTTPMockConnection(null, settings,
            "src/test/resources/http-mock-connection-test", "test", new HashMap<>(),
            mockContent);

        Assertions.assertEquals(Arrays.asList("application/json"), connectionContent.getHeaderFields().get("Accept"));
        Assertions.assertEquals("{}", getContent(connectionContent.getInputStream()));
        Assertions.assertThrows(IOException.class,
            () -> connectionContent.getOutputStream());
        Assertions.assertThrows(IOException.class,
            () -> connectionContent.getResponseCode());

        HTTPMockEntry mockFile = new HTTPMockEntry();
        mockFile.setResponseFilePath("{{mockResponseFilePath}}");

        HTTPMockConnection connectionFile = new HTTPMockConnection(null, settings,
            "src/test/resources/http-mock-connection-test", "test", new HashMap<>(),
            mockFile);

        Assertions.assertTrue(getContent(connectionFile.getInputStream()).contains("mockResponseField"));
    }

    private String getContent(InputStream inputStream) throws IOException {
        StringBuilder textBuilder = new StringBuilder();
        try (Reader reader = new BufferedReader(
            new InputStreamReader(inputStream, StandardCharsets.UTF_8))) {
            int c = 0;
            while ((c = reader.read()) != -1) {
                textBuilder.append((char) c);
            }
            return textBuilder.toString();
        }
    }
}
