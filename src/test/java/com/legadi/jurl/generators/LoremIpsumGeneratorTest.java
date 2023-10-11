package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.strip;
import static com.legadi.jurl.common.LoaderUtils.loadInternalLines;
import static com.legadi.jurl.model.GeneratorType.LOREM_IPSUM;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.CommandException;

public class LoremIpsumGeneratorTest extends GeneratorTest {

    public LoremIpsumGeneratorTest() {
        super(LOREM_IPSUM);
    }

    @Test
    public void loremIpsumDefault() {
        Set<String> lorem = new HashSet<>(loadInternalLines("lorem-ipsum.txt"));
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
        Set<String> lorem = new HashSet<>(loadInternalLines("lorem-ipsum.txt"));
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
