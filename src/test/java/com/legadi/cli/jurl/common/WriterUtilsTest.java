package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.JsonUtils.jsonToObject;
import static com.legadi.cli.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.cli.jurl.common.SettingsConstants.EXTERNAL_OS_NAME;
import static com.legadi.cli.jurl.common.WriterUtils.appendToFile;
import static com.legadi.cli.jurl.common.WriterUtils.cleanDirectory;
import static com.legadi.cli.jurl.common.WriterUtils.createDirectories;
import static com.legadi.cli.jurl.common.WriterUtils.deleteFileFromPath;
import static com.legadi.cli.jurl.common.WriterUtils.expandFile;
import static com.legadi.cli.jurl.common.WriterUtils.printFile;
import static com.legadi.cli.jurl.common.WriterUtils.writeFile;
import static com.legadi.cli.jurl.common.WriterUtils.writeLine;
import static com.legadi.cli.jurl.embedded.util.FileLoader.readLines;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.cli.jurl.embedded.model.BasicFunctionsEntity;
import com.legadi.cli.jurl.embedded.wrong.FailedDeleteFileVisitor;
import com.legadi.cli.jurl.embedded.wrong.FailedFileSystemPath;
import com.legadi.cli.jurl.embedded.wrong.UnwritableOutputStream;
import com.legadi.cli.jurl.embedded.wrong.UnwritableWriter;

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
    public void writeLineDataOutputStreamDefault() throws IOException {
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

            Assertions.assertEquals("Test line..." + System.lineSeparator(), writer.toString());
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

        expandFile(settings, input, output, new HashMap<>());

        BasicFunctionsEntity entity = loadJsonFile(output.toString(), new TypeToken<BasicFunctionsEntity>() {}, null);

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

        expandFile(settings, input, output, new HashMap<>(), line -> json.append(line));

        BasicFunctionsEntity jsonEntity = jsonToObject(json.toString(), new TypeToken<BasicFunctionsEntity>() {});
        BasicFunctionsEntity fileEntity = loadJsonFile(output.toString(), new TypeToken<BasicFunctionsEntity>() {}, null);

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
            () -> expandFile(settings, input, output, new HashMap<>()));
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

    @Test
    public void cleanDirectoryWalkFilesAll() {
        Settings settings = new Settings();
        Path cleanPath = settings.getExecutionPath()
            .resolve("src/test/resources/writer-utils/clean")
            .resolve(UUID.randomUUID().toString());
        Path emptyPath = cleanPath.resolve("empty");
        Pair<Path, Path[]> folderPaths = createFiles(cleanPath);

        createDirectories(emptyPath);
        Assertions.assertTrue(emptyPath.toFile().exists());
        Assertions.assertTrue(emptyPath.toFile().isDirectory());

        Assertions.assertDoesNotThrow(() -> cleanDirectory(cleanPath, new DeleteFileVisitor(null)));

        Assertions.assertFalse(emptyPath.toFile().exists());
        Assertions.assertFalse(folderPaths.getLeft().toFile().exists());

        for(int i = 0; i < folderPaths.getRight().length; i++) {
            Assertions.assertFalse(folderPaths.getRight()[i].toFile().exists());
        }
    }

    @Test
    public void cleanDirectoryWalkFilesUntilDateFuture() {
        Settings settings = new Settings();
        Path cleanPath = settings.getExecutionPath()
            .resolve("src/test/resources/writer-utils/clean")
            .resolve(UUID.randomUUID().toString());
        Pair<Path, Path[]> folderPaths = createFiles(cleanPath);

        LocalDate futureUntilDate = LocalDate.now().plusDays(2);
        Assertions.assertDoesNotThrow(() -> cleanDirectory(cleanPath, new DeleteFileVisitor(futureUntilDate)));

        Assertions.assertTrue(folderPaths.getLeft().toFile().exists());

        Assertions.assertDoesNotThrow(() -> cleanDirectory(cleanPath, new DeleteFileVisitor(null)));

        Assertions.assertFalse(folderPaths.getLeft().toFile().exists());

        for(int i = 0; i < folderPaths.getRight().length; i++) {
            Assertions.assertFalse(folderPaths.getRight()[i].toFile().exists());
        }
    }

    @Test
    public void cleanDirectoryWalkFilesUntilDateBefore() {
        Settings settings = new Settings();
        Path cleanPath = settings.getExecutionPath()
            .resolve("src/test/resources/writer-utils/clean")
            .resolve(UUID.randomUUID().toString());
        Pair<Path, Path[]> folderPaths = createFiles(cleanPath);

        LocalDate futureUntilDate = LocalDate.now().minusDays(2);
        Assertions.assertDoesNotThrow(() -> cleanDirectory(cleanPath, new DeleteFileVisitor(futureUntilDate)));

        Assertions.assertFalse(folderPaths.getLeft().toFile().exists());

        for(int i = 0; i < folderPaths.getRight().length; i++) {
            Assertions.assertFalse(folderPaths.getRight()[i].toFile().exists());
        }
    }

    @Test
    public void cleanDirectoryWalkFilesUntilDateNow() {
        Settings settings = new Settings();
        Path cleanPath = settings.getExecutionPath()
            .resolve("src/test/resources/writer-utils/clean")
            .resolve(UUID.randomUUID().toString());
        Pair<Path, Path[]> folderPaths = createFiles(cleanPath);

        LocalDate futureUntilDate = LocalDate.now();
        Assertions.assertDoesNotThrow(() -> cleanDirectory(cleanPath, new DeleteFileVisitor(futureUntilDate)));

        Assertions.assertFalse(folderPaths.getLeft().toFile().exists());

        for(int i = 0; i < folderPaths.getRight().length; i++) {
            Assertions.assertFalse(folderPaths.getRight()[i].toFile().exists());
        }
    }

    @Test
    public void cleanDirectoryNullDirectory() {
        Assertions.assertDoesNotThrow(() -> cleanDirectory(null, null));
    }

    @Test
    public void cleanDirectoryNotFound() {
        Settings settings = new Settings();
        Path cleanPath = settings.getExecutionPath()
            .resolve("src/test/resources/writer-utils/clean")
            .resolve(UUID.randomUUID().toString());

        Assertions.assertDoesNotThrow(() -> cleanDirectory(cleanPath, null));
    }

    @Test
    public void cleanDirectoryFailed() {
        Settings settings = new Settings();
        Path cleanPath = settings.getExecutionPath()
            .resolve("src/test/resources/writer-utils/clean")
            .resolve(UUID.randomUUID().toString());

        createDirectories(cleanPath);

        Assertions.assertThrows(IllegalStateException.class,
            () -> cleanDirectory(cleanPath, new FailedDeleteFileVisitor(null)));
    }

    @Test
    public void cleanDirectoryDiscardSymbolicLink() throws IOException {
        Settings settings = new Settings();
        Path deletePath = settings.getExecutionPath()
            .resolve("src/test/resources/writer-utils/clean")
            .resolve(UUID.randomUUID().toString());

        createDirectories(deletePath);

        String os = System.getProperty(EXTERNAL_OS_NAME).toLowerCase(Locale.ROOT);

        if(os.contains("linux")) {
            Path targetPath = deletePath.resolve("file.txt");
            Path linkPath = deletePath.resolve("symbolic_link.txt");

            Files.createSymbolicLink(linkPath, targetPath);

            Assertions.assertDoesNotThrow(() -> cleanDirectory(deletePath, new DeleteFileVisitor(null)));

            Assertions.assertTrue(deletePath.toFile().exists());
            Assertions.assertFalse(targetPath.toFile().exists());
            Assertions.assertTrue(linkPath.toFile().delete());
        }

        Assertions.assertDoesNotThrow(() -> cleanDirectory(deletePath, new DeleteFileVisitor(null)));

        Assertions.assertFalse(deletePath.toFile().exists());
    }

    @Test
    public void deleteFileFromPathValidation() {
        Settings settings = new Settings();
        Path deletePath = settings.getExecutionPath()
            .resolve("src/test/resources/writer-utils/clean")
            .resolve(UUID.randomUUID().toString());

        createDirectories(deletePath);

        boolean result = deleteFileFromPath(deletePath);

        Assertions.assertTrue(result);
    }

    @Test
    public void deleteFileFromPathNull() {
        boolean result = deleteFileFromPath(null);

        Assertions.assertFalse(result);
    }

    private Pair<Path, Path[]> createFiles(Path basePath) {
        Path folderPath = basePath.resolve("folder");
        Path[] filePaths = new Path[5];

        createDirectories(folderPath);
        Assertions.assertTrue(folderPath.toFile().exists());
        Assertions.assertTrue(folderPath.toFile().isDirectory());

        for(int i = 0; i < filePaths.length; i++) {
            String name = UUID.randomUUID().toString() + ".txt";
            filePaths[i] = folderPath.resolve(name);
            writeFile(filePaths[i], name);

            Assertions.assertTrue(filePaths[i].toFile().exists());
            Assertions.assertTrue(filePaths[i].toFile().isFile());
        }

        return new Pair<>(folderPath, filePaths);
    }
}
