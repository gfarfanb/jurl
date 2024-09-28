package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.LoaderUtils.loadAndCacheInternalLines;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class FullNameGeneratorTest extends GeneratorAbstractTest {

    private static final int WHITESPACE_LENGTH = 1;

    public FullNameGeneratorTest() {
        super("FULL_NAME");
    }

    @Test
    public void fullNameValidation() {
        Set<String> manNames = new HashSet<>(loadAndCacheInternalLines("man-names.txt"));
        Set<String> lastNames = new HashSet<>(loadAndCacheInternalLines("last-names.txt"));
        String value = generate();

        Assertions.assertNotNull(value);
        Assertions.assertTrue(value.contains(" "));

        int whitespaceIndex = value.lastIndexOf(" ");
        String manName = value.substring(0, whitespaceIndex);
        String lastName = value.substring(whitespaceIndex + WHITESPACE_LENGTH, value.length());

        Assertions.assertTrue(manNames.contains(manName));
        Assertions.assertTrue(lastNames.contains(lastName));
    }

    @Test
    public void fullNameGender() {
        Set<String> womanNames = new HashSet<>(loadAndCacheInternalLines("woman-names.txt"));
        Set<String> lastNames = new HashSet<>(loadAndCacheInternalLines("last-names.txt"));
        String value = generate("WOMAN");

        Assertions.assertNotNull(value);
        Assertions.assertTrue(value.contains(" "));

        int whitespaceIndex = value.lastIndexOf(" ");
        String womanName = value.substring(0, whitespaceIndex);
        String lastName = value.substring(whitespaceIndex + WHITESPACE_LENGTH, value.length());

        Assertions.assertTrue(womanNames.contains(womanName));
        Assertions.assertTrue(lastNames.contains(lastName));

        Set<String> manNames = new HashSet<>(loadAndCacheInternalLines("man-names.txt"));
        String valueOther = generate("OTHER");

        Assertions.assertNotNull(valueOther);
        Assertions.assertTrue(valueOther.contains(" "));

        int otherWhitespaceIndex = valueOther.lastIndexOf(" ");
        String otherName = valueOther.substring(0, otherWhitespaceIndex);
        String otherLastName = valueOther.substring(otherWhitespaceIndex + WHITESPACE_LENGTH, valueOther.length());

        Assertions.assertTrue(manNames.contains(otherName));
        Assertions.assertTrue(lastNames.contains(otherLastName));
    }
}
