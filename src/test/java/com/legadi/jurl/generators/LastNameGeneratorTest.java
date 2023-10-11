package com.legadi.jurl.generators;

import static com.legadi.jurl.common.LoaderUtils.loadInternalLines;
import static com.legadi.jurl.model.GeneratorType.LAST_NAME;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class LastNameGeneratorTest extends GeneratorTest {

    public LastNameGeneratorTest() {
        super(LAST_NAME);
    }

    @Test
    public void lastNameDefault() {
        Set<String> lastNames = new HashSet<>(loadInternalLines("last-names.txt"));
        String value = generate();

        Assertions.assertNotNull(value);
        Assertions.assertTrue(lastNames.contains(value));
    }
}