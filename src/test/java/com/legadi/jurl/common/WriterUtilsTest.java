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
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.WatchEvent.Kind;
import java.nio.file.WatchEvent.Modifier;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.embedded.model.BasicFunctionsEntity;

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
    public void expandFileDefault() {
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
    public void createDirectoriesDefault() throws IOException {
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
            () -> createDirectories(new FailedPath(directoryPath)));
    }

    @Test
    public void printFileDefault() {
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

    public static class UnwritableOutputStream extends OutputStream {

        @Override
        public void write(int b) throws IOException {
            throw new IOException();
        }
    }

    public static class UnwritableWriter extends BufferedWriter {

        public UnwritableWriter(Writer out) {
            super(out);
        }

        @Override
        public void write(String str) throws IOException {
            throw new IOException();
        }
    }

    public static class FailedPath implements Path {

        private final Path path;

        public FailedPath(Path path) {
            this.path = path;
        }

        @Override
        public FileSystem getFileSystem() {
            throw new IllegalStateException("Trying getFileSystem(): " + path.getFileSystem());
        }

        @Override
        public boolean isAbsolute() {
            return path.isAbsolute();
        }

        @Override
        public Path getRoot() {
            return path.getRoot();
        }

        @Override
        public Path getFileName() {
            return path.getFileName();
        }

        @Override
        public Path getParent() {
            return path.getParent();
        }

        @Override
        public int getNameCount() {
            return path.getNameCount();
        }

        @Override
        public Path getName(int index) {
            return path.getName(index);
        }

        @Override
        public Path subpath(int beginIndex, int endIndex) {
            return path.subpath(beginIndex, endIndex);
        }

        @Override
        public boolean startsWith(Path other) {
            return path.startsWith(other);
        }

        @Override
        public boolean startsWith(String other) {
            return path.startsWith(other);
        }

        @Override
        public boolean endsWith(Path other) {
            return path.endsWith(other);
        }

        @Override
        public boolean endsWith(String other) {
            return path.endsWith(other);
        }

        @Override
        public Path normalize() {
            return path.normalize();
        }

        @Override
        public Path resolve(Path other) {
            return path.resolve(other);
        }

        @Override
        public Path resolve(String other) {
            return path.resolve(other);
        }

        @Override
        public Path resolveSibling(Path other) {
            return path.resolveSibling(other);
        }

        @Override
        public Path resolveSibling(String other) {
            return path.resolveSibling(other);
        }

        @Override
        public Path relativize(Path other) {
            return path.relativize(other);
        }

        @Override
        public URI toUri() {
            return path.toUri();
        }

        @Override
        public Path toAbsolutePath() {
            return path.toAbsolutePath();
        }

        @Override
        public Path toRealPath(LinkOption... options) throws IOException {
            return path.toRealPath(options);
        }

        @Override
        public File toFile() {
            return path.toFile();
        }

        @Override
        public WatchKey register(WatchService watcher, Kind<?>[] events, Modifier... modifiers) throws IOException {
            return path.register(watcher, events, modifiers);
        }

        @Override
        public WatchKey register(WatchService watcher, Kind<?>... events) throws IOException {
            return path.register(watcher, events);
        }

        @Override
        public Iterator<Path> iterator() {
            return path.iterator();
        }

        @Override
        public int compareTo(Path other) {
            return path.compareTo(other);
        }
    }
}
