package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SAVE_OUTPUT_IN_LOCATION;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class DownloadInOptionTest extends OptionAbstractTest<DownloadInOption> {

    public DownloadInOptionTest() {
        super("--download-in");
    }

    @Test
    public void downloadInValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-di",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, RESPONSE);

        Assertions.assertEquals(Boolean.TRUE.toString(), settings.get(PROP_SAVE_OUTPUT_IN_LOCATION));
        Assertions.assertTrue(settings.getDownloadsLocation().isEmpty());
        Assertions.assertEquals(201, response.getStatusCode());
    }
}
