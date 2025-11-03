package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class TimesRepeatOptionTest extends OptionAbstractTest<TimesRepeatOption> {

    public TimesRepeatOptionTest() {
        super("--times", false);
    }

    @Test
    public void executeNTimesValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-t", "5",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        List<HTTPResponseEntry> responses = requestCatcher.getAll(correlationId, RESPONSE);

        Assertions.assertEquals(5, responses.size());

        for(HTTPResponseEntry response : responses) {
            Assertions.assertEquals(201, response.getStatusCode());
        }
    }

    @Test
    public void wrongTimesDefinition() {
        Assertions.assertThrows(CommandException.class,
            () -> jurl(
                "-t", "five"
            ));
    }
}
