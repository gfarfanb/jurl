package com.legadi.jurl.generators;

import static com.legadi.jurl.common.LoaderUtils.loadInternalLines;
import static com.legadi.jurl.model.GeneratorType.NAME;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class NameGeneratorTest extends GeneratorTest {

    public NameGeneratorTest() {
        super(NAME);
    }

    @Test
    public void nameDefault() {
        Set<String> manNames = new HashSet<>(loadInternalLines("man-names.txt"));
        String value = generate();

        Assertions.assertNotNull(value);
        Assertions.assertTrue(manNames.contains(value));
    }

    @Test
    public void nameGender() {
        Set<String> womanNames = new HashSet<>(loadInternalLines("woman-names.txt"));
        String value = generate("WOMAN");

        Assertions.assertNotNull(value);
        Assertions.assertTrue(womanNames.contains(value));

        Set<String> manNames = new HashSet<>(loadInternalLines("man-names.txt"));
        String otherValue = generate("OTHER");

        Assertions.assertNotNull(otherValue);
        Assertions.assertTrue(manNames.contains(otherValue));
    }
}
