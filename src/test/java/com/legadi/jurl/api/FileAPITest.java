package com.legadi.jurl.api;

import static com.legadi.jurl.embedded.util.FileLoader.readLines;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.EmbeddedAPITest;
import com.legadi.jurl.embedded.model.FileFormEntity;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class FileAPITest extends EmbeddedAPITest {

    @Test
    public void upload() throws IOException {
        UUID uploadIdentifier = jurl("-n", "upload", "src/test/resources/file.json");
        HTTPRequestEntry uploadRequest = requestCatcher.get(new TypeToken<HTTPRequestEntry>() {}, uploadIdentifier);

        Assertions.assertEquals("src/test/resources/file.json", uploadRequest.getRequestPath());
        Assertions.assertEquals("upload", uploadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file", uploadRequest.getUrl());
        Assertions.assertNull(uploadRequest.getProtocol());
        Assertions.assertNull(uploadRequest.getDomain());
        Assertions.assertEquals(0, uploadRequest.getPort());
        Assertions.assertNull(uploadRequest.getBasePath());
        Assertions.assertNull(uploadRequest.getEndpoint());
        Assertions.assertEquals("POST", uploadRequest.getMethod());
        Assertions.assertTrue(uploadRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(2, uploadRequest.getHeaders().size());
        Assertions.assertEquals(StandardCharsets.UTF_8.name(), uploadRequest.getBodyCharset());
        Assertions.assertNull(uploadRequest.getBodyContent());
        Assertions.assertNull(uploadRequest.getBodyFilePath());
        Assertions.assertNotNull(uploadRequest.getRequestFile());
        Assertions.assertFalse(uploadRequest.getRequestFile().getFormData().isEmpty());
        Assertions.assertNotNull(uploadRequest.getRequestFile().getBoundary());
        Assertions.assertTrue(uploadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, uploadRequest.getAssertions().size());

        HTTPResponseEntry uploadResponse = requestCatcher.get(new TypeToken<HTTPResponseEntry>() {}, uploadIdentifier);
        FileFormEntity uploadEntity = requestCatcher.getLastSaved(new TypeToken<FileFormEntity>() {}).getRight();
        Settings uploadSettings = requestCatcher.get(new TypeToken<Settings>() {}, uploadIdentifier);

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
        Assertions.assertEquals("./executions/file_json/upload/" + uploadSettings.getExecutionTag() + ".response",
            uploadResponse.getResponsePath().toString());
        Assertions.assertEquals(201, uploadResponse.getStatusCode());
        Assertions.assertEquals(6, uploadResponse.getResponseHeaders().size());

        Optional<AssertionResult> uploadAssertionResult = requestCatcher.get(new TypeToken<Optional<AssertionResult>>() {}, uploadIdentifier);

        Assertions.assertTrue(uploadAssertionResult.isPresent());
        Assertions.assertEquals(1, uploadAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, uploadAssertionResult.get().getFailures());
        Assertions.assertFalse(uploadAssertionResult.get().isSkip());

        Path filePath = Paths.get("src/test/resources/file.csv");
        Path temporalFilePath = uploadSettings.getTemporalPath().resolve(uploadEntity.getFilename());
        List<String> fileContent = readLines(filePath);
        List<String> temporalContent = readLines(temporalFilePath);

        Assertions.assertLinesMatch(fileContent, temporalContent);
    }

    @Test
    public void uploadWithoutForm() throws IOException {
        UUID uploadIdentifier = jurl("-n", "uploadWithoutForm", "src/test/resources/file.json");
        HTTPRequestEntry uploadRequest = requestCatcher.get(new TypeToken<HTTPRequestEntry>() {}, uploadIdentifier);

        Assertions.assertEquals("src/test/resources/file.json", uploadRequest.getRequestPath());
        Assertions.assertEquals("uploadWithoutForm", uploadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file", uploadRequest.getUrl());
        Assertions.assertNull(uploadRequest.getProtocol());
        Assertions.assertNull(uploadRequest.getDomain());
        Assertions.assertEquals(0, uploadRequest.getPort());
        Assertions.assertNull(uploadRequest.getBasePath());
        Assertions.assertNull(uploadRequest.getEndpoint());
        Assertions.assertEquals("POST", uploadRequest.getMethod());
        Assertions.assertTrue(uploadRequest.getQueryParams().isEmpty());
        Assertions.assertEquals(2, uploadRequest.getHeaders().size());
        Assertions.assertEquals(StandardCharsets.UTF_8.name(), uploadRequest.getBodyCharset());
        Assertions.assertNull(uploadRequest.getBodyContent());
        Assertions.assertNull(uploadRequest.getBodyFilePath());
        Assertions.assertNotNull(uploadRequest.getRequestFile());
        Assertions.assertTrue(uploadRequest.getRequestFile().getFormData().isEmpty());
        Assertions.assertNotNull(uploadRequest.getRequestFile().getBoundary());
        Assertions.assertTrue(uploadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, uploadRequest.getAssertions().size());

        HTTPResponseEntry uploadResponse = requestCatcher.get(new TypeToken<HTTPResponseEntry>() {}, uploadIdentifier);
        FileFormEntity uploadEntity = requestCatcher.getLastSaved(new TypeToken<FileFormEntity>() {}).getRight();
        Settings uploadSettings = requestCatcher.get(new TypeToken<Settings>() {}, uploadIdentifier);

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
        Assertions.assertEquals("./executions/file_json/uploadWithoutForm/" + uploadSettings.getExecutionTag() + ".response",
            uploadResponse.getResponsePath().toString());
        Assertions.assertEquals(201, uploadResponse.getStatusCode());
        Assertions.assertEquals(6, uploadResponse.getResponseHeaders().size());

        Optional<AssertionResult> uploadAssertionResult = requestCatcher.get(new TypeToken<Optional<AssertionResult>>() {}, uploadIdentifier);

        Assertions.assertTrue(uploadAssertionResult.isPresent());
        Assertions.assertEquals(1, uploadAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, uploadAssertionResult.get().getFailures());
        Assertions.assertFalse(uploadAssertionResult.get().isSkip());

        Path filePath = Paths.get("src/test/resources/file.csv");
        Path temporalFilePath = uploadSettings.getTemporalPath().resolve(uploadEntity.getFilename());
        List<String> fileContent = readLines(filePath);
        List<String> temporalContent = readLines(temporalFilePath);

        Assertions.assertLinesMatch(fileContent, temporalContent);
    }

    @Test
    public void download() {
        UUID downloadIdentifier = jurl("-n", "download", "src/test/resources/file.json");
        HTTPRequestEntry downloadRequest = requestCatcher.get(new TypeToken<HTTPRequestEntry>() {}, downloadIdentifier);

        Assertions.assertEquals("src/test/resources/file.json", downloadRequest.getRequestPath());
        Assertions.assertEquals("download", downloadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file", downloadRequest.getUrl());
        Assertions.assertNull(downloadRequest.getProtocol());
        Assertions.assertNull(downloadRequest.getDomain());
        Assertions.assertEquals(0, downloadRequest.getPort());
        Assertions.assertNull(downloadRequest.getBasePath());
        Assertions.assertNull(downloadRequest.getEndpoint());
        Assertions.assertEquals("GET", downloadRequest.getMethod());
        Assertions.assertEquals(2, downloadRequest.getQueryParams().size());
        Assertions.assertTrue(downloadRequest.getHeaders().isEmpty());
        Assertions.assertEquals(StandardCharsets.UTF_8.name(), downloadRequest.getBodyCharset());
        Assertions.assertNull(downloadRequest.getBodyContent());
        Assertions.assertNull(downloadRequest.getBodyFilePath());
        Assertions.assertNull(downloadRequest.getRequestFile());
        Assertions.assertTrue(downloadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, downloadRequest.getAssertions().size());

        HTTPResponseEntry downloadResponse = requestCatcher.get(new TypeToken<HTTPResponseEntry>() {}, downloadIdentifier);

        Assertions.assertTrue(downloadResponse.getRequestUrl().startsWith("http://localhost:" + port + "/file"));
        Assertions.assertTrue(downloadResponse.getRequestUrl().contains("file=src/test/resources/file.csv"));
        Assertions.assertTrue(downloadResponse.getRequestUrl().contains("name=downloaded.csv"));
        Assertions.assertTrue(downloadResponse.getCurlCommand().contains("-X GET"));
        Assertions.assertEquals("HTTP/1.1 200", downloadResponse.getResult());
        Assertions.assertEquals("./executions/file_json/download/downloaded.csv",
            downloadResponse.getResponsePath().toString());
        Assertions.assertEquals(200, downloadResponse.getStatusCode());
        Assertions.assertEquals(5, downloadResponse.getResponseHeaders().size());

        Optional<AssertionResult> downloadAssertionResult = requestCatcher.get(new TypeToken<Optional<AssertionResult>>() {}, downloadIdentifier);

        Assertions.assertTrue(downloadAssertionResult.isPresent());
        Assertions.assertEquals(1, downloadAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, downloadAssertionResult.get().getFailures());
        Assertions.assertFalse(downloadAssertionResult.get().isSkip());

        File downloadedFile = downloadResponse.getResponsePath().toFile();

        Assertions.assertTrue(downloadedFile.exists());
        Assertions.assertTrue(downloadedFile.length() > 0);
    }

    @Test
    public void downloadWithoutName() {
        UUID downloadIdentifier = jurl("-n", "downloadWithoutName", "src/test/resources/file.json");
        HTTPRequestEntry downloadRequest = requestCatcher.get(new TypeToken<HTTPRequestEntry>() {}, downloadIdentifier);

        Assertions.assertEquals("src/test/resources/file.json", downloadRequest.getRequestPath());
        Assertions.assertEquals("downloadWithoutName", downloadRequest.getName());
        Assertions.assertEquals("http://localhost:" + port + "/file", downloadRequest.getUrl());
        Assertions.assertNull(downloadRequest.getProtocol());
        Assertions.assertNull(downloadRequest.getDomain());
        Assertions.assertEquals(0, downloadRequest.getPort());
        Assertions.assertNull(downloadRequest.getBasePath());
        Assertions.assertNull(downloadRequest.getEndpoint());
        Assertions.assertEquals("GET", downloadRequest.getMethod());
        Assertions.assertEquals(1, downloadRequest.getQueryParams().size());
        Assertions.assertTrue(downloadRequest.getHeaders().isEmpty());
        Assertions.assertEquals(StandardCharsets.UTF_8.name(), downloadRequest.getBodyCharset());
        Assertions.assertNull(downloadRequest.getBodyContent());
        Assertions.assertNull(downloadRequest.getBodyFilePath());
        Assertions.assertNull(downloadRequest.getRequestFile());
        Assertions.assertTrue(downloadRequest.getOutputMappings().isEmpty());
        Assertions.assertEquals(1, downloadRequest.getAssertions().size());

        HTTPResponseEntry downloadResponse = requestCatcher.get(new TypeToken<HTTPResponseEntry>() {}, downloadIdentifier);
        Settings downloadSettings = requestCatcher.get(new TypeToken<Settings>() {}, downloadIdentifier);

        Assertions.assertTrue(downloadResponse.getRequestUrl().startsWith("http://localhost:" + port + "/file"));
        Assertions.assertTrue(downloadResponse.getRequestUrl().contains("file=src/test/resources/file.csv"));
        Assertions.assertTrue(downloadResponse.getCurlCommand().contains("-X GET"));
        Assertions.assertEquals("HTTP/1.1 200", downloadResponse.getResult());
        Assertions.assertEquals("./executions/file_json/downloadWithoutName/" + downloadSettings.getExecutionTag() + ".response",
            downloadResponse.getResponsePath().toString());
        Assertions.assertEquals(200, downloadResponse.getStatusCode());
        Assertions.assertEquals(5, downloadResponse.getResponseHeaders().size());

        Optional<AssertionResult> downloadAssertionResult = requestCatcher.get(new TypeToken<Optional<AssertionResult>>() {}, downloadIdentifier);

        Assertions.assertTrue(downloadAssertionResult.isPresent());
        Assertions.assertEquals(1, downloadAssertionResult.get().getAssertions());
        Assertions.assertEquals(0, downloadAssertionResult.get().getFailures());
        Assertions.assertFalse(downloadAssertionResult.get().isSkip());

        File downloadedFile = downloadResponse.getResponsePath().toFile();

        Assertions.assertTrue(downloadedFile.exists());
        Assertions.assertTrue(downloadedFile.length() > 0);
    }
}
