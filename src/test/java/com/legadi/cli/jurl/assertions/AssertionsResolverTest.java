package com.legadi.cli.jurl.assertions;

import static com.legadi.cli.jurl.assertions.AssertionsResolver.evaluate;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;
import com.legadi.cli.jurl.exception.AssertionException;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.model.AssertionEntry;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.AssertionType;

public class AssertionsResolverTest {

    @Test
    public void evaluateValidation() {
        Settings settings = new Settings();
        List<AssertionEntry> entries = new ArrayList<>();
        entries.add(instance("EQUALS_TO", "test", "test"));
        entries.add(instance("==", "test", "test"));

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> evaluate(settings, entries));

        Assertions.assertTrue(result.isPresent());
        Assertions.assertTrue(result.get().isPassed());
        Assertions.assertEquals(entries.size(), result.get().getAssertions());
        Assertions.assertEquals(0, result.get().getFailures());
        Assertions.assertTrue(result.get().getFailedMessages().isEmpty());
    }

    @Test
    public void evaluateWithValues() {
        Settings settings = new Settings();
        List<AssertionEntry> entries = new ArrayList<>();
        entries.add(instance("EQUALS_TO", "{{param}}", "test"));
        entries.add(instance("==", "test", "{{param}}"));

        Map<String, String> values = new HashMap<>();
        values.put("param", "test");

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> evaluate(settings, values, entries));

        Assertions.assertTrue(result.isPresent());
        Assertions.assertTrue(result.get().isPassed());
        Assertions.assertEquals(entries.size(), result.get().getAssertions());
        Assertions.assertEquals(0, result.get().getFailures());
        Assertions.assertTrue(result.get().getFailedMessages().isEmpty());
    }

    @Test
    public void evaluateFailed() {
        Settings settings = new Settings();
        List<AssertionEntry> entries = new ArrayList<>();
        entries.add(instance("EQUALS_TO", "test", "test1"));

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> evaluate(settings, entries));

        Assertions.assertTrue(result.isPresent());
        Assertions.assertFalse(result.get().isPassed());
        Assertions.assertEquals(entries.size(), result.get().getAssertions());
        Assertions.assertEquals(1, result.get().getFailures());
        Assertions.assertFalse(result.get().getFailedMessages().isEmpty());
    }

    @Test
    public void evaluateNotFound() {
        Settings settings = new Settings();
        List<AssertionEntry> entries = new ArrayList<>();
        entries.add(instance("NOT_FOUND", "test", "test"));

        Assertions.assertThrows(CommandException.class,
            () -> evaluate(settings, entries));
    }

    @Test
    public void evaluateWithMessage() {
        Settings settings = new Settings();
        List<AssertionEntry> entries = new ArrayList<>();
        AssertionEntry entry = instance("EQUALS_TO", "test", "test");

        entry.setMessage("Test");

        entries.add(entry);

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> evaluate(settings, entries));

        Assertions.assertTrue(result.isPresent());
        Assertions.assertTrue(result.get().isPassed());
        Assertions.assertEquals(entries.size(), result.get().getAssertions());
        Assertions.assertEquals(0, result.get().getFailures());
        Assertions.assertTrue(result.get().getFailedMessages().isEmpty());
    }

    @Test
    public void evaluateWithAssertionClass() {
        Settings settings = new Settings();
        List<AssertionEntry> entries = new ArrayList<>();
        AssertionEntry entry = instance(null);

        entry.setAssertionClass(ResolverAssertionFunction.class.getName());

        entries.add(entry);

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> evaluate(settings, entries));

        Assertions.assertTrue(result.isPresent());
        Assertions.assertTrue(result.get().isPassed());
        Assertions.assertEquals(entries.size(), result.get().getAssertions());
        Assertions.assertEquals(0, result.get().getFailures());
        Assertions.assertTrue(result.get().getFailedMessages().isEmpty());
    }

    @Test
    public void evaluateEmpty() {
        Settings settings = new Settings();

         Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> evaluate(settings, null));

        Assertions.assertFalse(result.isPresent());
    }

    private AssertionEntry instance(String name, String... args) {
        AssertionEntry instance = new AssertionEntry();
        instance.setName(name);
        instance.setArgs(args);
        instance.setType(AssertionType.ASSERTION);
        return instance;
    }

    @Named(name = "resolver")
    public static class ResolverAssertionFunction implements AssertionFunction {

        @Override
        public String[] getArgs() {
            return new String[0];
        }

        @Override
        public boolean apply(String[] args) throws AssertionException {
            return true;
        }
    }
}
