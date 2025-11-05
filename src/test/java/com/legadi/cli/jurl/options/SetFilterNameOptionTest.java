package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;

public class SetFilterNameOptionTest extends OptionAbstractTest<SetFilterNameOption> {

    public SetFilterNameOptionTest() {
        super(SetFilterNameOption.class, false);
    }

    @Test
    public void setFilter() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-f", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);

        Assertions.assertEquals("create", settings.getFilterName());
    }
}
