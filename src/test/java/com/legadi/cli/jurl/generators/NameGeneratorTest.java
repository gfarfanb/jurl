package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.LoaderUtils.loadAndCacheInternalLines;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class NameGeneratorTest extends GeneratorAbstractTest {

    public NameGeneratorTest() {
        super("NAME");
    }

    @Test
    public void nameValidation() {
        Set<String> manNames = new HashSet<>(loadAndCacheInternalLines("man-names.txt"));
        String value = generate();

        Assertions.assertNotNull(value);
        Assertions.assertTrue(manNames.contains(value));
    }

    @Test
    public void nameGender() {
        Set<String> womanNames = new HashSet<>(loadAndCacheInternalLines("woman-names.txt"));
        String value = generate("WOMAN");

        Assertions.assertNotNull(value);
        Assertions.assertTrue(womanNames.contains(value));

        Set<String> manNames = new HashSet<>(loadAndCacheInternalLines("man-names.txt"));
        String otherValue = generate("OTHER");

        Assertions.assertNotNull(otherValue);
        Assertions.assertTrue(manNames.contains(otherValue));
    }
}
