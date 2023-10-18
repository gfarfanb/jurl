package com.legadi.jurl.common;

import static com.legadi.jurl.common.JsonUtils.jsonToObject;
import static com.legadi.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.jurl.common.WriterUtils.appendToFile;
import static com.legadi.jurl.common.WriterUtils.createDirectories;
import static com.legadi.jurl.common.WriterUtils.expandFile;
import static com.legadi.jurl.common.WriterUtils.printFile;
import static com.legadi.jurl.common.WriterUtils.writeFile;
import static com.legadi.jurl.common.WriterUtils.writeLine;
import static com.legadi.jurl.embedded.util.FileLoader.readLines;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.embedded.model.BasicFunctionsEntity;
import com.legadi.jurl.embedded.wrong.FailedFileSystemPath;
import com.legadi.jurl.embedded.wrong.UnwritableOutputStream;
import com.legadi.jurl.embedded.wrong.UnwritableWriter;

public class WriterUtilsTest {

    @Test
    public void writeLineDataOutputStream() throws IOException {
        try(ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                DataOutputStream dataOutputStream = new DataOutputStream(outputStream)) {

            writeLine(dataOutputStream, "Test line...", StandardCharsets.UTF_8.name());

            Assertions.assertEquals("Test line...", outputStream.toString());
        }
    }

    @Test
    public void writeLineDataOutputStreamUnwritable() throws IOException {
        try(UnwritableOutputStream outputStream = new UnwritableOutputStream();
                DataOutputStream dataOutputStream = new DataOutputStream(outputStream)) {

            Assertions.assertThrows(IllegalStateException.class,
                () -> writeLine(dataOutputStream, "Test line...", StandardCharsets.UTF_8.name()));
        }
    }

    @Test
    public void writeLineBufferedWriter() throws IOException {
        try(StringWriter writer = new StringWriter();
                BufferedWriter bufferedWriter = new BufferedWriter(writer)) {

            writeLine(bufferedWriter, "Test line...");
            bufferedWriter.flush();

            Assertions.assertEquals("Test line...\n", writer.toString());
        }
    }

    @Test
    public void writeLineBufferedWriterUnwritable() throws IOException {
        try(StringWriter writer = new StringWriter();
                UnwritableWriter bufferedWriter = new UnwritableWriter(writer)) {

            Assertions.assertThrows(IllegalStateException.class,
                () -> writeLine(bufferedWriter, "Test line..."));
        }
    }

    @Test
    public void writeFileLines() throws IOException {
        Settings settings = new Settings();
        Path file = createDirectories(
                settings.getExecutionPath().resolve("src/test/resources/writer-utils")
            )
            .resolve(UUID.randomUUID() + ".txt");
        String[] lines = { "A,B,C", "a,b,c", "1,2,3" };

        writeFile(file, lines);

        Set<String> fileContent = new HashSet<>(readLines(file));

        for(String line : lines) {
            Assertions.assertTrue(fileContent.contains(line));
        }
    }

    @Test
    public void writeFileDirectoryNotFound() throws IOException {
        Settings settings = new Settings();
        Path file = settings.getExecutionPath()
            .resolve("directory/not/found/writer-utils")
            .resolve(UUID.randomUUID() + ".txt");
        String[] lines = { "A,B,C", "a,b,c", "1,2,3" };

        Assertions.assertThrows(IllegalStateException.class,
            () -> writeFile(file, lines));
    }

    @Test
    public void appendFileLines() throws IOException {
        Settings settings = new Settings();
        Path file = createDirectories(
                settings.getExecutionPath().resolve("src/test/resources/writer-utils")
            )
            .resolve(UUID.randomUUID() + ".txt");
        String[] lines = { "A,B,C", "a,b,c", "1,2,3" };

        writeFile(file, lines);
        appendToFile(file.toFile(), file.toFile().length(), "", "x,y,z");

        Set<String> fileContent = new HashSet<>(readLines(file));

        for(String line : lines) {
            Assertions.assertTrue(fileContent.contains(line));
        }

        Assertions.assertTrue(fileContent.contains("x,y,z"));
    }

    @Test
    public void appendFileDirectoryNotFound() throws IOException {
        Settings settings = new Settings();
        Path file = settings.getExecutionPath()
            .resolve("directory/not/found/writer-utils")
            .resolve(UUID.randomUUID() + ".txt");

        Assertions.assertThrows(IllegalStateException.class,
            () -> appendToFile(file.toFile(), file.toFile().length(), "", "x,y,z"));
    }

    @Test
    public void expandFileValidation() {
        Settings settings = new Settings();
        Path input = Paths.get("src/test/resources/basic-functions.body.generators.txt");
        Path output = createDirectories(
                settings.getExecutionPath().resolve("src/test/resources/writer-utils")
            )
            .resolve(UUID.randomUUID() + ".json");

        expandFile(settings, input, output);

        BasicFunctionsEntity entity = loadJsonFile(output.toString(), new TypeToken<BasicFunctionsEntity>() {});

        Assertions.assertNotNull(entity);
        Assertions.assertNotNull(entity.getAccess());
        Assertions.assertNotNull(entity.getName());
        Assertions.assertNotNull(entity.getEmail());
        Assertions.assertNotNull(entity.getNickname());
        Assertions.assertNotNull(entity.getAmount());
        Assertions.assertTrue(entity.getCoins() > 0);
        Assertions.assertNotNull(entity.getBio());
        Assertions.assertNotNull(entity.getType());
        Assertions.assertNotNull(entity.getTimestamp());
    }

    @Test
    public void expandFileConsumer() {
        Settings settings = new Settings();
        Path input = Paths.get("src/test/resources/basic-functions.body.generators.txt");
        Path output = createDirectories(
                settings.getExecutionPath().resolve("src/test/resources/writer-utils")
            )
            .resolve(UUID.randomUUID() + ".json");
        StringBuilder json = new StringBuilder();

        expandFile(settings, input, output, line -> json.append(line));

        BasicFunctionsEntity jsonEntity = jsonToObject(json.toString(), new TypeToken<BasicFunctionsEntity>() {});
        BasicFunctionsEntity fileEntity = loadJsonFile(output.toString(), new TypeToken<BasicFunctionsEntity>() {});

        Assertions.assertNotNull(jsonEntity);
        Assertions.assertNotNull(fileEntity);

        Assertions.assertEquals(jsonEntity.getAccess(), fileEntity.getAccess());
        Assertions.assertEquals(jsonEntity.getName(), fileEntity.getName());
        Assertions.assertEquals(jsonEntity.getEmail(), fileEntity.getEmail());
        Assertions.assertEquals(jsonEntity.getNickname(), fileEntity.getNickname());
        Assertions.assertEquals(jsonEntity.getAmount(), fileEntity.getAmount());
        Assertions.assertEquals(jsonEntity.isActive(), fileEntity.isActive());
        Assertions.assertEquals(jsonEntity.getCoins(), fileEntity.getCoins());
        Assertions.assertEquals(jsonEntity.getBio(), fileEntity.getBio());
        Assertions.assertEquals(jsonEntity.getType(), fileEntity.getType());
        Assertions.assertEquals(jsonEntity.getTimestamp(), fileEntity.getTimestamp());
    }

    @Test
    public void expandFileFailed() {
        Settings settings = new Settings();
        Path input = Paths.get("src/test/resources/basic-functions.body.generators.txt");
        Path output = settings.getExecutionPath()
            .resolve("directory/not/found/writer-utils")
            .resolve(UUID.randomUUID() + ".json");

        Assertions.assertThrows(IllegalStateException.class,
            () -> expandFile(settings, input, output));
    }

    @Test
    public void createDirectoriesValidation() throws IOException {
        Settings settings = new Settings();
        Path directoryPath = settings.getExecutionPath()
            .resolve("src/test/resources/writer-utils/directories")
            .resolve(UUID.randomUUID().toString());

        Assertions.assertDoesNotThrow(() -> createDirectories(directoryPath));
        Assertions.assertTrue(Files.isDirectory(directoryPath));

        Files.delete(directoryPath);
    }

    @Test
    public void createDirectoriesFailed() throws IOException {
        Settings settings = new Settings();
        Path directoryPath = settings.getExecutionPath()
            .resolve("src/test/resources/writer-utils/directories")
            .resolve(UUID.randomUUID().toString());

        Assertions.assertThrows(IllegalStateException.class,
            () -> createDirectories(new FailedFileSystemPath(directoryPath)));
    }

    @Test
    public void printFileValidation() {
        Path file = Paths.get("src/test/resources/basic-functions.body.generators.txt");

        Assertions.assertDoesNotThrow(() -> printFile(file));
    }

    @Test
    public void printFileFailed() {
        Settings settings = new Settings();
        Path file = settings.getExecutionPath()
            .resolve("directory/not/found/writer-utils")
            .resolve(UUID.randomUUID() + ".txt");

        Assertions.assertThrows(IllegalStateException.class,
            () -> printFile(file));
    }

    @Test
    public void printFileNull() {
        Path filePath = null;
        String file = null;

        Assertions.assertDoesNotThrow(() -> printFile(filePath));
        Assertions.assertDoesNotThrow(() -> printFile(file));
    }
}
