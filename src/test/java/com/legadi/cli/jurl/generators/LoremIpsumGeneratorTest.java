package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.CommonUtils.strip;
import static com.legadi.cli.jurl.common.LoaderUtils.loadAndCacheInternalLines;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.CommandException;

public class LoremIpsumGeneratorTest extends GeneratorAbstractTest {

    public LoremIpsumGeneratorTest() {
        super("LOREM_IPSUM");
    }

    @Test
    public void loremIpsumValidation() {
        Set<String> lorem = new HashSet<>(loadAndCacheInternalLines("lorem-ipsum.txt"));
        String value = generate();

        Assertions.assertNotNull(value);

        String[] words = value.split(" ");

        Assertions.assertEquals(LoremIpsumGenerator.DEFAULT_WORDS, words.length);

        for(String word : words) {
            Assertions.assertTrue(lorem.contains(strip(word.toLowerCase(), ".")));
        }
    }

    @Test
    public void loremIpsumLength() {
        Set<String> lorem = new HashSet<>(loadAndCacheInternalLines("lorem-ipsum.txt"));
        String value = generate("1000");

        Assertions.assertNotNull(value);

        String[] words = value.split(" ");

        Assertions.assertEquals(1000, words.length);

        for(String word : words) {
            Assertions.assertTrue(lorem.contains(strip(word.toLowerCase(), ".")));
        }
    }

    @Test
    public void loremIpsumWrongArg() {
        Assertions.assertThrows(CommandException.class,
            () -> generate("words"));
    }
}
