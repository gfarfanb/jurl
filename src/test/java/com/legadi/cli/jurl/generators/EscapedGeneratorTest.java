package com.legadi.cli.jurl.generators;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.legadi.cli.jurl.generators.EscapedGenerator.EscapedChars;

import java.util.stream.Stream;
import java.util.Arrays;

public class EscapedGeneratorTest extends GeneratorAbstractTest {

    public EscapedGeneratorTest() {
        super("ESCAPED");
    }

    @ParameterizedTest
    @MethodSource("escapedValidationArgs")
    public void escapedValidation(String param, String expected) {
        String escaped = Assertions.assertDoesNotThrow(() -> generate(param));

        Assertions.assertEquals(expected, escaped);
    }

    @Test
    public void invalidEscaped() {
        String escaped = generate("UNKNOWN");

        Assertions.assertEquals("UNKNOWN", escaped);
    }

    private static Stream<Arguments> escapedValidationArgs() {
        return Arrays.stream(EscapedChars.values())
            .map(escaped -> Arguments.of(escaped.name().toLowerCase(), escaped.getReplacement()));
    }
}
