package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;

public class SetInputNameOptionTest extends OptionAbstractTest<SetInputNameOption> {

    public SetInputNameOptionTest() {
        super("--name", false);
    }

    @Test
    public void setName() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);

        Assertions.assertEquals("create", settings.getInputName());
    }
}
