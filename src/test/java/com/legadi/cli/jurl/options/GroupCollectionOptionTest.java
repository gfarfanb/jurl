package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.CommonUtils.toGroupParam;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;

public class GroupCollectionOptionTest extends OptionAbstractTest<GroupCollectionOption> {

    public GroupCollectionOptionTest() {
        super("--group-coll");
    }

    @Test
    public void groupCollectionValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-gc", "test-group", "LAST",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);
        String groupName = toGroupParam(Optional.of(settings), "test-group");

        Assertions.assertEquals("LAST", settings.get(groupName));
    }
}
