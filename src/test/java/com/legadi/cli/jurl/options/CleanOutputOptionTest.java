package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.WriterUtils.writeFile;

import java.nio.file.Path;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.SkipExecutionException;

public class CleanOutputOptionTest extends OptionAbstractTest<CleanOutputOption> {

    public CleanOutputOptionTest() {
        super("--remove", false);
    }

    @Test
    public void cleanAllFileS() {
        Settings settings = new Settings();
        Path file = settings.getExecutionPath().resolve(UUID.randomUUID().toString());

        writeFile(file, "To Delete");

        Assertions.assertTrue(file.toFile().exists());

        Assertions.assertThrows(SkipExecutionException.class,
            () -> jurl(
                "-rm", "all",
                "src/test/resources/basic-functions.spec.http"
            ));

        Assertions.assertFalse(file.toFile().exists());
    }

    @Test
    public void cleanFilesUntilDate() {
        Settings settings = new Settings();
        Path file = settings.getExecutionPath().resolve(UUID.randomUUID().toString());
        String untilDate = DateTimeFormatter.ISO_LOCAL_DATE.format(LocalDate.now().minusDays(1));

        writeFile(file, "To Delete");

        Assertions.assertTrue(file.toFile().exists());

        Assertions.assertThrows(SkipExecutionException.class,
            () -> jurl(
                "-rm", untilDate,
                "basic-functios.spec.http"
            ));

        Assertions.assertFalse(file.toFile().exists());
    }
}
