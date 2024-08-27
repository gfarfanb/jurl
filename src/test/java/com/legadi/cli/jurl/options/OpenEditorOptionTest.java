package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_OPEN_OUTPUT_IN_EDITOR;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class OpenEditorOptionTest extends OptionTest<OpenEditorOption> {

    public OpenEditorOptionTest() {
        super("--open-editor");
    }

    @Test
    public void openEditorValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-oe",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, "settings");
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, "response");

        Assertions.assertEquals(Boolean.TRUE.toString(), settings.get(PROP_OPEN_OUTPUT_IN_EDITOR));
        Assertions.assertTrue(settings.getOpenEditorCommand().isEmpty());
        Assertions.assertEquals(201, response.getStatusCode());
    }
}
