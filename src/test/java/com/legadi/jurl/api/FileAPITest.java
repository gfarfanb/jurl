package com.legadi.jurl.api;

import static com.legadi.jurl.embedded.util.FileLoader.readLines;
import static com.legadi.jurl.executor.http.HTTPRequestExecutor.REQUEST_FILE_BOUNDARY;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.EmbeddedAPITest;
import com.legadi.jurl.embedded.model.FileFormEntity;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class FileAPITest extends EmbeddedAPITest {

    @Test
    public void upload() throws IOException {
        UUID uploadCorrelationId = jurl("-n", "upload", "src/test/resources/file.spec.http");
        Settings uploadSettings = requestCatcher.get(uploadCorrelationId, "settings");
        String requestInputPath = requestCatcher.get(uploadCorrelationId, "request-input-path");
        HTTPRequestEntry uploadRequest = requestCatcher.get(uploadCorrelationId, "request");

        Assertions.assertEquals("src/test/resources/file.spec.http", requestInputPath);
        Assertions.assertEquals("upload", uploadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file", uploadRequest.getUrl());
        Assertions.assertNull(uploadRequest.getProtocol());
        Assertions.assertNull(uploadRequest.getHost());
        Assertions.assertNull(null, uploadRequest.getPort());
        Assertions.assertNull(uploadRequest.getBasePath());
        Assertions.assertNull(uploadRequest.getEndpoint());
        Assertions.assertEquals("POST", uploadRequest.getMethod());
        Assertions.assertTrue(uploadRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(3, uploadRequest.getHeaders().size());
        Assertions.assertNull(uploadRequest.getBodyCharset());
        Assertions.assertNull(uploadRequest.getBodyContent());
        Assertions.assertNull(uploadRequest.getBodyFilePath());
        Assertions.assertNotNull(uploadRequest.getRequestFile());
        Assertions.assertFalse(uploadRequest.getRequestFile().getFormData().isEmpty());
        Assertions.assertDoesNotThrow(() -> uploadSettings.get(REQUEST_FILE_BOUNDARY));
        Assertions.assertTrue(uploadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, uploadRequest.getAssertions().size());

        HTTPResponseEntry uploadResponse = requestCatcher.get(uploadCorrelationId, "response");
        FileFormEntity uploadEntity = requestCatcher
            .<FileFormEntity>getLastSaved("file-body")
            .getRight();

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
        Assertions.assertEquals("./executions/src/test/resources/file_spec_http/upload/"
            + uploadSettings.getTimestamp().toLocalDate() + "/"
            + uploadSettings.getExecutionTag() + ".response",
            uploadResponse.getResponsePath().toString());
        Assertions.assertEquals(201, uploadResponse.getStatusCode());
        Assertions.assertFalse(uploadResponse.getResponseHeaders().isEmpty());
        Assertions.assertTrue(uploadResponse.getResponseHeaders().containsKey("Resource-ID"));
        Assertions.assertDoesNotThrow(
            () -> UUID.fromString(uploadResponse.getResponseHeaders().get("Resource-ID")));

        Optional<AssertionResult> uploadAssertionResult = requestCatcher.get(uploadCorrelationId, "assertion-result");

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
        Settings uploadSettings = requestCatcher.get(uploadCorrelationId, "settings");
        String requestInputPath = requestCatcher.get(uploadCorrelationId, "request-input-path");
        HTTPRequestEntry uploadRequest = requestCatcher.get(uploadCorrelationId, "request");

        Assertions.assertEquals("src/test/resources/file.spec.http", requestInputPath);
        Assertions.assertEquals("uploadWithoutForm", uploadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file", uploadRequest.getUrl());
        Assertions.assertNull(uploadRequest.getProtocol());
        Assertions.assertNull(uploadRequest.getHost());
        Assertions.assertNull(null, uploadRequest.getPort());
        Assertions.assertNull(uploadRequest.getBasePath());
        Assertions.assertNull(uploadRequest.getEndpoint());
        Assertions.assertEquals("POST", uploadRequest.getMethod());
        Assertions.assertTrue(uploadRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(3, uploadRequest.getHeaders().size());
        Assertions.assertNull(uploadRequest.getBodyCharset());
        Assertions.assertNull(uploadRequest.getBodyContent());
        Assertions.assertNull(uploadRequest.getBodyFilePath());
        Assertions.assertNotNull(uploadRequest.getRequestFile());
        Assertions.assertTrue(uploadRequest.getRequestFile().getFormData().isEmpty());
        Assertions.assertDoesNotThrow(() -> uploadSettings.get(REQUEST_FILE_BOUNDARY));
        Assertions.assertTrue(uploadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, uploadRequest.getAssertions().size());

        HTTPResponseEntry uploadResponse = requestCatcher.get(uploadCorrelationId, "response");
        FileFormEntity uploadEntity = requestCatcher
            .<FileFormEntity>getLastSaved("file-body")
            .getRight();

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
        Assertions.assertEquals("./executions/src/test/resources/file_spec_http/uploadWithoutForm/"
            + uploadSettings.getTimestamp().toLocalDate() + "/"
            + uploadSettings.getExecutionTag() + ".response",
            uploadResponse.getResponsePath().toString());
        Assertions.assertEquals(201, uploadResponse.getStatusCode());
        Assertions.assertFalse(uploadResponse.getResponseHeaders().isEmpty());
        Assertions.assertTrue(uploadResponse.getResponseHeaders().containsKey("Resource-ID"));
        Assertions.assertDoesNotThrow(
            () -> UUID.fromString(uploadResponse.getResponseHeaders().get("Resource-ID")));

        Optional<AssertionResult> uploadAssertionResult = requestCatcher.get(uploadCorrelationId, "assertion-result");

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
        String requestInputPath = requestCatcher.get(downloadCorrelationId, "request-input-path");
        HTTPRequestEntry downloadRequest = requestCatcher.get(downloadCorrelationId, "request");

        Assertions.assertEquals("src/test/resources/file.spec.http", requestInputPath);
        Assertions.assertEquals("download", downloadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file", downloadRequest.getUrl());
        Assertions.assertNull(downloadRequest.getProtocol());
        Assertions.assertNull(downloadRequest.getHost());
        Assertions.assertNull(downloadRequest.getPort());
        Assertions.assertNull(downloadRequest.getBasePath());
        Assertions.assertNull(downloadRequest.getEndpoint());
        Assertions.assertEquals("GET", downloadRequest.getMethod());
        Assertions.assertEquals(2, downloadRequest.getQueryParams().size());
        Assertions.assertEquals(1, downloadRequest.getHeaders().size());
        Assertions.assertNull(downloadRequest.getBodyCharset());
        Assertions.assertNull(downloadRequest.getBodyContent());
        Assertions.assertNull(downloadRequest.getBodyFilePath());
        Assertions.assertNull(downloadRequest.getRequestFile());
        Assertions.assertTrue(downloadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, downloadRequest.getAssertions().size());

        HTTPResponseEntry downloadResponse = requestCatcher.get(downloadCorrelationId, "response");
        Settings downloadSettings = requestCatcher.get(downloadCorrelationId, "settings");

        Assertions.assertTrue(downloadResponse.getRequestUrl().startsWith("http://localhost:" + port + "/file"));
        Assertions.assertTrue(downloadResponse.getRequestUrl().contains("file=src/test/resources/file.csv"));
        Assertions.assertTrue(downloadResponse.getRequestUrl().contains("name=downloaded.csv"));
        Assertions.assertTrue(downloadResponse.getCurlCommand().contains("-X GET"));
        Assertions.assertEquals("HTTP/1.1 200", downloadResponse.getResult());
        Assertions.assertEquals("./executions/src/test/resources/file_spec_http/download/"
            + downloadSettings.getTimestamp().toLocalDate() + "/downloaded.csv",
            downloadResponse.getResponsePath().toString());
        Assertions.assertEquals(200, downloadResponse.getStatusCode());
        Assertions.assertFalse(downloadResponse.getResponseHeaders().isEmpty());
        Assertions.assertTrue(downloadResponse.getResponseHeaders().containsKey("Content-Disposition"));
        Assertions.assertEquals("attachment; filename=downloaded.csv",
            downloadResponse.getResponseHeaders().get("Content-Disposition"));

        Optional<AssertionResult> downloadAssertionResult = requestCatcher.get(downloadCorrelationId, "assertion-result");

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
        String requestInputPath = requestCatcher.get(downloadCorrelationId, "request-input-path");
        HTTPRequestEntry downloadRequest = requestCatcher.get(downloadCorrelationId, "request");

        Assertions.assertEquals("src/test/resources/file.spec.http", requestInputPath);
        Assertions.assertEquals("downloadWithoutName", downloadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file?", downloadRequest.getUrl());
        Assertions.assertNull(downloadRequest.getProtocol());
        Assertions.assertNull(downloadRequest.getHost());
        Assertions.assertNull(downloadRequest.getPort());
        Assertions.assertNull(downloadRequest.getBasePath());
        Assertions.assertNull(downloadRequest.getEndpoint());
        Assertions.assertEquals("GET", downloadRequest.getMethod());
        Assertions.assertEquals(1, downloadRequest.getQueryParams().size());
        Assertions.assertEquals(1, downloadRequest.getHeaders().size());
        Assertions.assertNull(downloadRequest.getBodyCharset());
        Assertions.assertNull(downloadRequest.getBodyContent());
        Assertions.assertNull(downloadRequest.getBodyFilePath());
        Assertions.assertNull(downloadRequest.getRequestFile());
        Assertions.assertTrue(downloadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, downloadRequest.getAssertions().size());

        HTTPResponseEntry downloadResponse = requestCatcher.get(downloadCorrelationId, "response");
        Settings downloadSettings = requestCatcher.get(downloadCorrelationId, "settings");

        Assertions.assertTrue(downloadResponse.getRequestUrl().startsWith("http://localhost:" + port + "/file"));
        Assertions.assertTrue(downloadResponse.getRequestUrl().contains("file=src/test/resources/file.csv"));
        Assertions.assertTrue(downloadResponse.getCurlCommand().contains("-X GET"));
        Assertions.assertEquals("HTTP/1.1 200", downloadResponse.getResult());
        Assertions.assertEquals("./executions/src/test/resources/file_spec_http/downloadWithoutName/"
            + downloadSettings.getTimestamp().toLocalDate() + "/"
            + downloadSettings.getExecutionTag() + ".response",
            downloadResponse.getResponsePath().toString());
        Assertions.assertEquals(200, downloadResponse.getStatusCode());
        Assertions.assertFalse(downloadResponse.getResponseHeaders().isEmpty());
        Assertions.assertTrue(downloadResponse.getResponseHeaders().containsKey("Content-Disposition"));
        Assertions.assertEquals("attachment",
            downloadResponse.getResponseHeaders().get("Content-Disposition"));

        Optional<AssertionResult> downloadAssertionResult = requestCatcher.get(downloadCorrelationId, "assertion-result");

        Assertions.assertTrue(downloadAssertionResult.isPresent());
        Assertions.assertEquals(1, downloadAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, downloadAssertionResult.get().getFailures());
        Assertions.assertTrue(downloadAssertionResult.get().isPassed());

        File downloadedFile = downloadResponse.getResponsePath().toFile();

        Assertions.assertTrue(downloadedFile.exists());
        Assertions.assertTrue(downloadedFile.length() > 0);
    }
}
