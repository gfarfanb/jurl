package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.LoaderUtils.loadAndCacheInternalLines;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.CommandException;

public class EmailGeneratorTest extends GeneratorAbstractTest {

    private static final int AT_LENGTH = 1;

    public EmailGeneratorTest() {
        super("EMAIL");
    }

    @Test
    public void emailValidation() {
        Set<String> domains = new HashSet<>(loadAndCacheInternalLines("domains.txt"));
        String value = generate();

        Assertions.assertNotNull(value);
        Assertions.assertTrue(value.contains("@"));

        int atIndex = value.lastIndexOf("@");
        String part = value.substring(0, atIndex);
        String domain = value.substring(atIndex + AT_LENGTH, value.length());

        Assertions.assertTrue(part.length() >= EmailGenerator.RANGE_LENGTH);
        Assertions.assertTrue(domains.contains(domain));
    }

    @Test
    public void emailLength() {
        Set<String> domains = new HashSet<>(loadAndCacheInternalLines("domains.txt"));
        String value = generate("20");

        Assertions.assertNotNull(value);
        Assertions.assertTrue(value.contains("@"));

        int atIndex = value.lastIndexOf("@");
        String part = value.substring(0, atIndex);
        String domain = value.substring(atIndex + AT_LENGTH, value.length());

        Assertions.assertEquals(20, part.length());
        Assertions.assertTrue(domains.contains(domain));
    }

    @Test
    public void emailWrongArg() {
        Assertions.assertThrows(CommandException.class,
            () -> generate("domain"));
    }
}
