package com.legadi.cli.jurl.options;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.SkipExecutionException;

public class ListRequestsOptionTest extends OptionAbstractTest<ListRequestsOption> {

    public ListRequestsOptionTest() {
        super("--list-all", false);
    }

    @Test
    public void listAllValidation() {
        Assertions.assertThrows(SkipExecutionException.class,
                () -> jurl( "-s", "workspacePath", "src/test/resources/requests", "-la"));
    }
}
