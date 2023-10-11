package com.legadi.jurl.generators;

import static com.legadi.jurl.model.GeneratorType.PICK_ANY;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class PickAnyGeneratorTest extends GeneratorTest {

    public PickAnyGeneratorTest() {
        super(PICK_ANY);
    }

    @Test
    public void pickAnyDefault() {
        String value = generate();

        Assertions.assertNotNull(value);
        Assertions.assertEquals("", value);
    }

    @Test
    public void pickAnyValues() {
        Set<String> values = new HashSet<>(Arrays.asList("A", "B", "C", "D", "E"));
        String value = generate("A,B,C,D,E");

        Assertions.assertNotNull(value);
        Assertions.assertTrue(values.contains(value));

        String valueUnique = generate("UNIQUE");

        Assertions.assertNotNull(valueUnique);
        Assertions.assertEquals("UNIQUE", valueUnique);

        String valueWhitespace = generate("     ");

        Assertions.assertNotNull(valueWhitespace);
        Assertions.assertEquals("", valueWhitespace);
    }
}
