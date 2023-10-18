package com.legadi.jurl.common;

import static com.legadi.jurl.common.LoaderUtils.instantiate;
import static com.legadi.jurl.common.LoaderUtils.loadAndCacheInternalLines;
import static com.legadi.jurl.common.LoaderUtils.loadCredentials;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.Credential;

public class LoaderUtilsTest {

    @Test
    public void loadCredentialsValidation() {
        Path credentialsPath = Paths.get("src/test/resources/credentials.json");

        Map<String, Credential> credentials = Assertions.assertDoesNotThrow(
            () -> loadCredentials(credentialsPath));

        Assertions.assertFalse(credentials.isEmpty());
        Assertions.assertEquals("test", credentials.get("test").getId());
        Assertions.assertEquals("test", credentials.get("test").getUsername());
        Assertions.assertEquals("7e5t", credentials.get("test").getPassword());
        Assertions.assertEquals("bearer", credentials.get("test").getType());
        Assertions.assertEquals("n34i8423493n294n2349h02e42304h", credentials.get("test").getToken());
        Assertions.assertEquals("u23h283h293eh9e", credentials.get("test").getKey());
        Assertions.assertEquals("39h2r8h83fw27g7", credentials.get("test").getClient());
        Assertions.assertEquals("h283he823he239dg4u7t92uye", credentials.get("test").getSecret());
        Assertions.assertEquals("1234", credentials.get("test").getPin());
    }

    @Test
    public void loadCredentialsFileNotFound() {
        Path credentialsPath = Paths.get("credentials-file-not-found.json");

        Map<String, Credential> credentials = Assertions.assertDoesNotThrow(
            () -> loadCredentials(credentialsPath));

        Assertions.assertTrue(credentials.isEmpty());
    }

    @Test
    public void loadCredentialsDuplicated() {
        Path credentialsPath = Paths.get("src/test/resources/credentials.duplicated.json");

        Assertions.assertThrows(CommandException.class,
            () -> loadCredentials(credentialsPath));
    }

    @Test
    public void loadAndCacheInternalLinesValidation() {
        List<String> lines = Assertions.assertDoesNotThrow(
            () -> loadAndCacheInternalLines("lines.txt"));

        Assertions.assertEquals(3, lines.size());

        List<String> cachedLines = Assertions.assertDoesNotThrow(
            () -> loadAndCacheInternalLines("lines.txt"));

        Assertions.assertEquals(3, cachedLines.size());
    }

    @Test
    public void loadAndCacheInternalLinesFileNotFound() {
        Assertions.assertThrows(IllegalStateException.class,
            () -> loadAndCacheInternalLines("lines-file-not-found.txt"));
    }

    @Test
    public void instantiateValidation() {
        TestLoader instance = Assertions.assertDoesNotThrow(
            () -> instantiate(TestLoader.class.getName()));

        Assertions.assertNotNull(instance);
    }

    @Test
    public void instantiateConstructorWithArguments() {
        Assertions.assertThrows(IllegalStateException.class,
            () -> instantiate(TestLoaderArgs.class.getName()));
    }

    public static class TestLoader {

    }

    public static class TestLoaderArgs {

        public TestLoaderArgs(String name) {}
    }
}
