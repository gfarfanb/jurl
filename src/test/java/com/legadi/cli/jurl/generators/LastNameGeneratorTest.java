package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.LoaderUtils.loadAndCacheInternalLines;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class LastNameGeneratorTest extends GeneratorAbstractTest {

    public LastNameGeneratorTest() {
        super("LAST_NAME");
    }

    @Test
    public void lastNameValidation() {
        Set<String> lastNames = new HashSet<>(loadAndCacheInternalLines("last-names.txt"));
        String value = generate();

        Assertions.assertNotNull(value);
        Assertions.assertTrue(lastNames.contains(value));
    }
}
