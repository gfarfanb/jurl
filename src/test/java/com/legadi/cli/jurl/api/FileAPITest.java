package com.legadi.cli.jurl.api;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_DOWNLOADS_LOCATION;
import static com.legadi.cli.jurl.embedded.util.FileLoader.readLines;
import static com.legadi.cli.jurl.embedded.util.FilenameUtils.toSystemSeparator;
import static com.legadi.cli.jurl.embedded.util.ObjectName.ASSERTIONS_RESULT;
import static com.legadi.cli.jurl.embedded.util.ObjectName.BODY;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST_INPUT_PATH;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;
import static com.legadi.cli.jurl.executor.http.HTTPRequestExecutor.REQUEST_FILE_BOUNDARY;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.EmbeddedAPIAbstractTest;
import com.legadi.cli.jurl.embedded.model.FileFormEntity;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class FileAPITest extends EmbeddedAPIAbstractTest {

    @Test
    public void upload() throws IOException {
        UUID uploadCorrelationId = jurl("-n", "upload", "src/test/resources/file.spec.http");
        Settings uploadSettings = requestCatcher.getLast(uploadCorrelationId, SETTINGS);
        String requestInputPath = requestCatcher.getLast(uploadCorrelationId, REQUEST_INPUT_PATH);
        HTTPRequestEntry uploadRequest = requestCatcher.getLast(uploadCorrelationId, REQUEST);

        Assertions.assertEquals("src/test/resources/file.spec.http", requestInputPath);
        Assertions.assertEquals("upload", uploadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file", uploadRequest.getUrl());
        Assertions.assertEquals("POST", uploadRequest.getMethod());
        Assertions.assertTrue(uploadRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(3, uploadRequest.getHeaders().size());
        Assertions.assertNull(uploadRequest.getBodyCharset());
        Assertions.assertNull(uploadRequest.getBodyContent());
        Assertions.assertNull(uploadRequest.getBodyFilePath());
        Assertions.assertFalse(uploadRequest.getRequestFiles().isEmpty());
        Assertions.assertFalse(uploadRequest.getFormData().isEmpty());
        Assertions.assertDoesNotThrow(() -> uploadSettings.get(REQUEST_FILE_BOUNDARY));
        Assertions.assertTrue(uploadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, uploadRequest.getAssertions().size());

        HTTPResponseEntry uploadResponse = requestCatcher.getLast(uploadCorrelationId, RESPONSE);
        FileFormEntity uploadEntity = (FileFormEntity) requestCatcher
            .getLastSaved(BODY)
            .getRight();
        String expectedResponsePath = toSystemSeparator(
            "./executions/src/test/resources/file_spec_http/upload/"
            + uploadSettings.getTimestamp().toLocalDate() + "/"
            + uploadSettings.getExecutionTag() + ".response"
        );

        Assertions.assertEquals("http://localhost:" + port + "/file", uploadResponse.getRequestUrl());
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains("-X POST"));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains("-H \"Connection: Keep-Alive\""));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains("-H \"Content-Type: multipart/form-data; boundary="));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains("@src/test/resources/file.csv"));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains("http://localhost:" + port + "/file"));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains(uploadEntity.getIdentifier().toString()));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains(DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(uploadEntity.getTimestamp())));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains(uploadEntity.getFilename()));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains(uploadEntity.getType()));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains(uploadEntity.getField()));
        Assertions.assertEquals("HTTP/1.1 201", uploadResponse.getResult());
        Assertions.assertEquals(expectedResponsePath, uploadResponse.getResponsePath().toString());
        Assertions.assertEquals(201, uploadResponse.getStatusCode());
        Assertions.assertFalse(uploadResponse.getResponseHeaders().isEmpty());
        Assertions.assertTrue(uploadResponse.getResponseHeaders().containsKey("Resource-ID"));
        Assertions.assertDoesNotThrow(
            () -> UUID.fromString(uploadResponse.getResponseHeaders().get("Resource-ID")));

        Optional<AssertionResult> uploadAssertionResult = requestCatcher.getLast(uploadCorrelationId, ASSERTIONS_RESULT);

        Assertions.assertTrue(uploadAssertionResult.isPresent());
        Assertions.assertEquals(1, uploadAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, uploadAssertionResult.get().getFailures());
        Assertions.assertTrue(uploadAssertionResult.get().isPassed());

        Path filePath = Paths.get("src/test/resources/file.csv");
        Path temporalFilePath = uploadSettings.getExecutionPath().resolve(uploadEntity.getFilename());
        List<String> fileContent = readLines(filePath);
        List<String> temporalContent = readLines(temporalFilePath);

        Assertions.assertLinesMatch(fileContent, temporalContent);
    }

    @Test
    public void uploadWithoutForm() throws IOException {
        UUID uploadCorrelationId = jurl("-n", "uploadWithoutForm", "src/test/resources/file.spec.http");
        Settings uploadSettings = requestCatcher.getLast(uploadCorrelationId, SETTINGS);
        String requestInputPath = requestCatcher.getLast(uploadCorrelationId, REQUEST_INPUT_PATH);
        HTTPRequestEntry uploadRequest = requestCatcher.getLast(uploadCorrelationId, REQUEST);

        Assertions.assertEquals("src/test/resources/file.spec.http", requestInputPath);
        Assertions.assertEquals("uploadWithoutForm", uploadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file", uploadRequest.getUrl());
        Assertions.assertEquals("POST", uploadRequest.getMethod());
        Assertions.assertTrue(uploadRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(3, uploadRequest.getHeaders().size());
        Assertions.assertNull(uploadRequest.getBodyCharset());
        Assertions.assertNull(uploadRequest.getBodyContent());
        Assertions.assertNull(uploadRequest.getBodyFilePath());
        Assertions.assertFalse(uploadRequest.getRequestFiles().isEmpty());
        Assertions.assertTrue(uploadRequest.getFormData().isEmpty());
        Assertions.assertDoesNotThrow(() -> uploadSettings.get(REQUEST_FILE_BOUNDARY));
        Assertions.assertTrue(uploadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, uploadRequest.getAssertions().size());

        HTTPResponseEntry uploadResponse = requestCatcher.getLast(uploadCorrelationId, RESPONSE);
        FileFormEntity uploadEntity = (FileFormEntity) requestCatcher
            .getLastSaved(BODY)
            .getRight();
        String expectedResponsePath = toSystemSeparator(
            "./executions/src/test/resources/file_spec_http/uploadWithoutForm/"
            + uploadSettings.getTimestamp().toLocalDate() + "/"
            + uploadSettings.getExecutionTag() + ".response"
        );

        Assertions.assertEquals("http://localhost:" + port + "/file", uploadResponse.getRequestUrl());
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains("-X POST"));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains("-H \"Connection: Keep-Alive\""));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains("-H \"Content-Type: multipart/form-data; boundary="));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains("@src/test/resources/file.csv"));
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains("http://localhost:" + port + "/file"));
        Assertions.assertNull(uploadEntity.getIdentifier());
        Assertions.assertNull(uploadEntity.getTimestamp());
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains(uploadEntity.getFilename()));
        Assertions.assertNull(uploadEntity.getType());
        Assertions.assertTrue(uploadResponse.getCurlCommand().contains(uploadEntity.getField()));
        Assertions.assertEquals("HTTP/1.1 201", uploadResponse.getResult());
        Assertions.assertEquals(expectedResponsePath, uploadResponse.getResponsePath().toString());
        Assertions.assertEquals(201, uploadResponse.getStatusCode());
        Assertions.assertFalse(uploadResponse.getResponseHeaders().isEmpty());
        Assertions.assertTrue(uploadResponse.getResponseHeaders().containsKey("Resource-ID"));
        Assertions.assertDoesNotThrow(
            () -> UUID.fromString(uploadResponse.getResponseHeaders().get("Resource-ID")));

        Optional<AssertionResult> uploadAssertionResult = requestCatcher.getLast(uploadCorrelationId, ASSERTIONS_RESULT);

        Assertions.assertTrue(uploadAssertionResult.isPresent());
        Assertions.assertEquals(1, uploadAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, uploadAssertionResult.get().getFailures());
        Assertions.assertTrue(uploadAssertionResult.get().isPassed());

        Path filePath = Paths.get("src/test/resources/file.csv");
        Path temporalFilePath = uploadSettings.getExecutionPath().resolve(uploadEntity.getFilename());
        List<String> fileContent = readLines(filePath);
        List<String> temporalContent = readLines(temporalFilePath);

        Assertions.assertLinesMatch(fileContent, temporalContent);
    }

    @Test
    public void download() {
        UUID downloadCorrelationId = jurl("-n", "download", "src/test/resources/file.spec.http");
        String requestInputPath = requestCatcher.getLast(downloadCorrelationId, REQUEST_INPUT_PATH);
        HTTPRequestEntry downloadRequest = requestCatcher.getLast(downloadCorrelationId, REQUEST);

        Assertions.assertEquals("src/test/resources/file.spec.http", requestInputPath);
        Assertions.assertEquals("download", downloadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file", downloadRequest.getUrl());
        Assertions.assertEquals("GET", downloadRequest.getMethod());
        Assertions.assertEquals(2, downloadRequest.getQueryParams().size());
        Assertions.assertEquals(1, downloadRequest.getHeaders().size());
        Assertions.assertNull(downloadRequest.getBodyCharset());
        Assertions.assertNull(downloadRequest.getBodyContent());
        Assertions.assertNull(downloadRequest.getBodyFilePath());
        Assertions.assertTrue(downloadRequest.getRequestFiles().isEmpty());
        Assertions.assertTrue(downloadRequest.getFormData().isEmpty());
        Assertions.assertTrue(downloadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, downloadRequest.getAssertions().size());

        HTTPResponseEntry downloadResponse = requestCatcher.getLast(downloadCorrelationId, RESPONSE);
        Settings downloadSettings = requestCatcher.getLast(downloadCorrelationId, SETTINGS);
        String expectedResponsePath = toSystemSeparator(
            "./executions/src/test/resources/file_spec_http/download/"
            + downloadSettings.getTimestamp().toLocalDate() + "/downloaded.csv"
        );

        Assertions.assertTrue(downloadResponse.getRequestUrl().startsWith("http://localhost:" + port + "/file"));
        Assertions.assertTrue(downloadResponse.getRequestUrl().contains("file=src/test/resources/file.csv"));
        Assertions.assertTrue(downloadResponse.getRequestUrl().contains("name=downloaded.csv"));
        Assertions.assertTrue(downloadResponse.getCurlCommand().contains("-X GET"));
        Assertions.assertEquals("HTTP/1.1 200", downloadResponse.getResult());
        Assertions.assertEquals(expectedResponsePath, downloadResponse.getResponsePath().toString());
        Assertions.assertEquals(200, downloadResponse.getStatusCode());
        Assertions.assertFalse(downloadResponse.getResponseHeaders().isEmpty());
        Assertions.assertTrue(downloadResponse.getResponseHeaders().containsKey("Content-Disposition"));
        Assertions.assertEquals("attachment; filename=downloaded.csv",
            downloadResponse.getResponseHeaders().get("Content-Disposition"));

        Optional<AssertionResult> downloadAssertionResult = requestCatcher.getLast(downloadCorrelationId, ASSERTIONS_RESULT);

        Assertions.assertTrue(downloadAssertionResult.isPresent());
        Assertions.assertEquals(1, downloadAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, downloadAssertionResult.get().getFailures());
        Assertions.assertTrue(downloadAssertionResult.get().isPassed());

        File downloadedFile = downloadResponse.getResponsePath().toFile();

        Assertions.assertTrue(downloadedFile.exists());
        Assertions.assertTrue(downloadedFile.length() > 0);
    }

    @Test
    public void downloadWithoutName() {
        UUID downloadCorrelationId = jurl("-n", "downloadWithoutName", "src/test/resources/file.spec.http");
        String requestInputPath = requestCatcher.getLast(downloadCorrelationId, REQUEST_INPUT_PATH);
        HTTPRequestEntry downloadRequest = requestCatcher.getLast(downloadCorrelationId, REQUEST);

        Assertions.assertEquals("src/test/resources/file.spec.http", requestInputPath);
        Assertions.assertEquals("downloadWithoutName", downloadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file?", downloadRequest.getUrl());
        Assertions.assertEquals("GET", downloadRequest.getMethod());
        Assertions.assertEquals(1, downloadRequest.getQueryParams().size());
        Assertions.assertEquals(1, downloadRequest.getHeaders().size());
        Assertions.assertNull(downloadRequest.getBodyCharset());
        Assertions.assertNull(downloadRequest.getBodyContent());
        Assertions.assertNull(downloadRequest.getBodyFilePath());
        Assertions.assertTrue(downloadRequest.getRequestFiles().isEmpty());
        Assertions.assertTrue(downloadRequest.getFormData().isEmpty());
        Assertions.assertTrue(downloadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, downloadRequest.getAssertions().size());

        HTTPResponseEntry downloadResponse = requestCatcher.getLast(downloadCorrelationId, RESPONSE);
        Settings downloadSettings = requestCatcher.getLast(downloadCorrelationId, SETTINGS);
        String expectedResponsePath = toSystemSeparator(
            "./executions/src/test/resources/file_spec_http/downloadWithoutName/"
            + downloadSettings.getTimestamp().toLocalDate() + "/"
            + downloadSettings.getExecutionTag() + ".response"
        );

        Assertions.assertTrue(downloadResponse.getRequestUrl().startsWith("http://localhost:" + port + "/file"));
        Assertions.assertTrue(downloadResponse.getRequestUrl().contains("file=src/test/resources/file.csv"));
        Assertions.assertTrue(downloadResponse.getCurlCommand().contains("-X GET"));
        Assertions.assertEquals("HTTP/1.1 200", downloadResponse.getResult());
        Assertions.assertEquals(expectedResponsePath, downloadResponse.getResponsePath().toString());
        Assertions.assertEquals(200, downloadResponse.getStatusCode());
        Assertions.assertFalse(downloadResponse.getResponseHeaders().isEmpty());
        Assertions.assertTrue(downloadResponse.getResponseHeaders().containsKey("Content-Disposition"));
        Assertions.assertEquals("attachment",
            downloadResponse.getResponseHeaders().get("Content-Disposition"));

        Optional<AssertionResult> downloadAssertionResult = requestCatcher.getLast(downloadCorrelationId, ASSERTIONS_RESULT);

        Assertions.assertTrue(downloadAssertionResult.isPresent());
        Assertions.assertEquals(1, downloadAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, downloadAssertionResult.get().getFailures());
        Assertions.assertTrue(downloadAssertionResult.get().isPassed());

        File downloadedFile = downloadResponse.getResponsePath().toFile();

        Assertions.assertTrue(downloadedFile.exists());
        Assertions.assertTrue(downloadedFile.length() > 0);
    }

    @Test
    public void downloadInLocation() throws IOException {
        Map<String, String> properties = new HashMap<>();
        String downloadsLocation = "./executions/Downloads";

        properties.put(PROP_DOWNLOADS_LOCATION, downloadsLocation);

        Settings.mergeProperties("default", properties);

        UUID downloadCorrelationId = jurl("-di", "-n", "downloadInLocation", "src/test/resources/file.spec.http");
        String requestInputPath = requestCatcher.getLast(downloadCorrelationId, REQUEST_INPUT_PATH);
        HTTPRequestEntry downloadRequest = requestCatcher.getLast(downloadCorrelationId, REQUEST);

        Assertions.assertEquals("src/test/resources/file.spec.http", requestInputPath);
        Assertions.assertEquals("downloadInLocation", downloadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file?", downloadRequest.getUrl());
        Assertions.assertEquals("GET", downloadRequest.getMethod());
        Assertions.assertEquals(2, downloadRequest.getQueryParams().size());
        Assertions.assertEquals(1, downloadRequest.getHeaders().size());
        Assertions.assertNull(downloadRequest.getBodyCharset());
        Assertions.assertNull(downloadRequest.getBodyContent());
        Assertions.assertNull(downloadRequest.getBodyFilePath());
        Assertions.assertTrue(downloadRequest.getRequestFiles().isEmpty());
        Assertions.assertTrue(downloadRequest.getFormData().isEmpty());
        Assertions.assertTrue(downloadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, downloadRequest.getAssertions().size());

        HTTPResponseEntry downloadResponse = requestCatcher.getLast(downloadCorrelationId, RESPONSE);
        String expectedResponsePath = toSystemSeparator(
            downloadsLocation + "/saved.csv"
        );

        Assertions.assertTrue(downloadResponse.getRequestUrl().startsWith("http://localhost:" + port + "/file"));
        Assertions.assertTrue(downloadResponse.getRequestUrl().contains("file=src/test/resources/file.csv"));
        Assertions.assertTrue(downloadResponse.getCurlCommand().contains("-X GET"));
        Assertions.assertEquals("HTTP/1.1 200", downloadResponse.getResult());
        Assertions.assertEquals(expectedResponsePath, downloadResponse.getResponsePath().toString());
        Assertions.assertEquals(200, downloadResponse.getStatusCode());
        Assertions.assertFalse(downloadResponse.getResponseHeaders().isEmpty());
        Assertions.assertTrue(downloadResponse.getResponseHeaders().containsKey("Content-Disposition"));
        Assertions.assertEquals("attachment; filename=saved.csv",
            downloadResponse.getResponseHeaders().get("Content-Disposition"));

        Optional<AssertionResult> downloadAssertionResult = requestCatcher.getLast(downloadCorrelationId, ASSERTIONS_RESULT);

        Assertions.assertTrue(downloadAssertionResult.isPresent());
        Assertions.assertEquals(1, downloadAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, downloadAssertionResult.get().getFailures());
        Assertions.assertTrue(downloadAssertionResult.get().isPassed());

        File downloadedFile = downloadResponse.getResponsePath().toFile();

        Assertions.assertTrue(downloadedFile.exists());
        Assertions.assertTrue(downloadedFile.length() > 0);
        Assertions.assertTrue(downloadedFile.delete());
        Assertions.assertFalse(downloadedFile.exists());

        properties.put(PROP_DOWNLOADS_LOCATION, "");

        Settings.mergeProperties("default", properties);
    }

    @Test
    public void uploadFiles() throws IOException {
        UUID uploadCorrelationId = jurl("-n", "uploadFiles", "src/test/resources/file.spec.http");

        HTTPResponseEntry uploadResponse = requestCatcher.getLast(uploadCorrelationId, RESPONSE);
        Settings uploadSettings = requestCatcher.getLast(uploadCorrelationId, SETTINGS);

        for(Path path : uploadResponse.getSentFilePaths()) {
            File uploadedFile = uploadSettings.getExecutionPath()
                .resolve(path.toFile().getName()).toFile();

            Assertions.assertTrue(uploadedFile.exists());
            Assertions.assertTrue(uploadedFile.length() > 0);
            Assertions.assertTrue(uploadedFile.delete());
            Assertions.assertFalse(uploadedFile.exists());
        }
    }

    @Test
    public void uploadFileNotFound() throws IOException {
        Assertions.assertThrows(IllegalStateException.class,
            () -> jurl("-n", "uploadFileNotFound", "src/test/resources/file.spec.http"));
    }
}
